-- DEFINE RELATIONS

CREATE TABLE IF NOT EXISTS Category (
    id SERIAL PRIMARY KEY
  , category VARCHAR(255) NOT NULL
  , supercategory INTEGER NOT NULL DEFAULT 1
                  REFERENCES Category (id) ON DELETE RESTRICT ON UPDATE CASCADE
  , UNIQUE (category, supercategory)
);

CREATE TABLE IF NOT EXISTS Transaction (
    id SERIAL PRIMARY KEY
  , amount INTEGER NOT NULL
  , category_id INTEGER NOT NULL
                REFERENCES Category (id) ON DELETE RESTRICT ON UPDATE CASCADE
  , date TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  , notes VARCHAR(255) NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS MonthBudget (
    id SERIAL PRIMARY KEY
  , year INTEGER NOT NULL
  , month SMALLINT NOT NULL
            CONSTRAINT valid_month
            CHECK (month > 0 AND month < 13)
  , category_id INTEGER NOT NULL
                REFERENCES Category (id) ON DELETE RESTRICT ON UPDATE CASCADE
  , planned INTEGER NOT NULL
  , actual INTEGER NOT NULL
  , UNIQUE (year, month, category_id)
);


-- CREATE TRIGGERS

CREATE OR REPLACE FUNCTION fn_update_real()
RETURNS TRIGGER AS $update_real$
BEGIN
  UPDATE MonthBudget SET actual = actual + NEW.amount
                   WHERE NEW.category_id = category_id
                     AND EXTRACT(YEAR FROM NEW.date) = MonthBudget.year
                     AND EXTRACT(MONTH FROM NEW.date) = MonthBudget.month;
  RETURN NEW;
END;
$update_real$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION fn_update_real2()
RETURNS TRIGGER AS $update_real2$
BEGIN
  IF NEW.amount != OLD.amount THEN
    UPDATE MonthBudget SET actual = actual + NEW.amount - OLD.amount
                     WHERE NEW.category_id = category_id
                       AND EXTRACT(YEAR FROM NEW.date) = MonthBudget.year
                       AND EXTRACT(MONTH FROM NEW.date) = MonthBudget.month;
  END IF;
  RETURN NEW;
END;
$update_real2$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER update_real
AFTER INSERT ON Transaction
FOR EACH ROW
EXECUTE PROCEDURE fn_update_real();

CREATE OR REPLACE TRIGGER update_real2
AFTER UPDATE OF amount ON Transaction
FOR EACH ROW
EXECUTE PROCEDURE fn_update_real2();


-- POPULATE DATABASE

BEGIN;
ALTER TABLE Category DISABLE TRIGGER ALL;
INSERT INTO Category (category, supercategory) VALUES ('root', 1);
ALTER TABLE Category ENABLE TRIGGER ALL;
COMMIT;

INSERT INTO Category (category, supercategory) VALUES
      ('egresos', 1)        -- 2
    , ('ingresos', 1)       -- 3
    , ('impuestos', 2)      -- 4
    , ('seguros', 2)        -- 5
    , ('vivienda', 2)       -- 6
    , ('suministros', 2)    -- 7
    , ('alimentos', 2)      -- 8
    , ('personal', 2)       -- 9
    , ('credito', 2)
    , ('inversiones', 3)
    , ('sueldo', 3)
    , ('bonos', 3)
    , ('intereses', 3)
    , ('prestamos', 3)
    , ('deudores', 3)
    , ('iva', 4)
    , ('isr', 4)
    , ('seguro_medico', 5)
    , ('seguro_desempleo', 5)
    , ('pension', 5)
    , ('renta', 6)
    , ('luz', 7)
    , ('gas', 7)
    , ('internet', 7)
    , ('telefonia', 7)
    , ('transporte', 9)
    , ('entretenimiento', 9)
    , ('classes', 9)
    , ('otros', 9);
