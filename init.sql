-- DEFINE RELATIONS

CREATE TABLE IF NOT EXISTS "Categories" (
      "category" VARCHAR(50) PRIMARY KEY
    , "supercategory" VARCHAR NOT NULL DEFAULT 'root'
                      REFERENCES "Categories" ("category")
                      ON DELETE RESTRICT
                      ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS "Transactions" (
      "id" SERIAL PRIMARY KEY
    , "amount" INTEGER NOT NULL
    , "category" VARCHAR(50) NOT NULL
                 REFERENCES "Categories" ("category")
                 ON DELETE RESTRICT
                 ON UPDATE CASCADE
    , "date" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    , "notes" VARCHAR(255) NOT NULL DEFAULT ''
);

CREATE TABLE IF NOT EXISTS "Months" (
      "id" SMALLSERIAL PRIMARY KEY
    , "year" SMALLINT NOT NULL
             DEFAULT EXTRACT(YEAR FROM CURRENT_TIMESTAMP)
    , "month" SMALLINT NOT NULL
              DEFAULT EXTRACT(MONTH FROM CURRENT_TIMESTAMP)
              CONSTRAINT "valid_month" CHECK ("month" > 0 AND "month" < 13)
    , "income" INTEGER NOT NULL DEFAULT 0
    , "expenses" INTEGER NOT NULL DEFAULT 0
    , "balance" INTEGER NOT NULL DEFAULT 0
    , UNIQUE ("year", "month")
);

CREATE TABLE IF NOT EXISTS "Balances" (
      "month_id" SMALLINT REFERENCES "Months" ("id")
                 ON DELETE RESTRICT
                 ON UPDATE CASCADE
    , "category" VARCHAR(50) NOT NULL
                    REFERENCES "Categories" ("category")
                    ON DELETE RESTRICT
                    ON UPDATE CASCADE
    , "planned" INTEGER NOT NULL DEFAULT 0
    , "actual" INTEGER NOT NULL DEFAULT 0
    , PRIMARY KEY ("month_id", "category")
);


-- CREATE FUNCTIONS AND TRIGGERS

CREATE OR REPLACE FUNCTION get_supercategories(cat text)
  RETURNS TABLE (category text)
AS $$
  WITH RECURSIVE root(category) AS (
    SELECT CAST ("category" AS TEXT) FROM "Categories" WHERE "category" = cat
    UNION ALL
      SELECT "Categories"."supercategory" FROM root, "Categories"
        WHERE root."category" = "Categories"."category"
        AND "Categories"."category" NOT IN ('root', 'income', 'expense')
  )
  SELECT * FROM root;
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION is_income(cat text)
  RETURNS BOOLEAN
AS $$
  SELECT (
    SELECT "category" FROM get_supercategories(cat) ORDER BY "category" ASC LIMIT 1
  ) = 'income';
$$ LANGUAGE SQL;

CREATE OR REPLACE FUNCTION is_expense(cat text)
  RETURNS BOOLEAN
AS $$
  SELECT (
    SELECT "category" FROM get_supercategories(cat) ORDER BY "category" ASC LIMIT 1
  ) = 'expense';
$$ LANGUAGE SQL;

CREATE OR REPLACE PROCEDURE update_months(transaction "Transactions")
AS $$
    from datetime import datetime

    date = datetime.strptime(transaction["date"], "%Y-%m-%d %H:%M:%S.%f")
    t_year = date.year
    t_month = date.month
    income = 0
    expenses = 0
    query = plpy.prepare("SELECT * FROM is_income($1)", ["text"])
    if plpy.execute(query, [transaction["category"]])[0]["is_income"]:
      income = transaction["amount"]
    else:
      expenses = transaction["amount"]
    balance = income - expenses

    month_query = plpy.prepare(
      'SELECT * FROM "Months" WHERE "year" = $1 AND "month" = $2',
      ["smallint", "smallint"]
    )
    months = plpy.execute(month_query, [t_year, t_month])
    if not months:
        query = plpy.prepare(
          'INSERT INTO "Months" ("year", "month", "income", "expenses", "balance") '
          'VALUES ($1, $2, $3, $4, $5)',
          ["smallint", "smallint", "integer", "integer", "integer"]
        )
        plpy.execute(query, [t_year, t_month, income, expenses, balance])
        month = plpy.execute(month_query, [t_year, t_month])[0]
    else:
        month = months[0]
        query = plpy.prepare(
          'UPDATE "Months" SET "income" = "income" + $1, "expenses" = "expenses" + $2, '
          '"balance" = "balance" + $3 WHERE "id" = $4',
          ["integer", "integer", "integer", "integer"]
        )
        plpy.execute(query, [income, expenses, balance, month["id"]])
$$ LANGUAGE plpython3u;

CREATE OR REPLACE PROCEDURE update_balances(transaction "Transactions")
AS $$
    from datetime import datetime

    date = datetime.strptime(transaction["date"], "%Y-%m-%d %H:%M:%S.%f")
    t_month = date.month
    t_year = date.year
    query = plpy.prepare(
      'SELECT * FROM "Months" WHERE "year" = $1 AND "month" = $2',
      ["smallint", "smallint"]
    )
    month = plpy.execute(query, [t_year, t_month])[0]
    query = plpy.prepare('SELECT * FROM get_supercategories($1)', ["text"])
    supercategories = plpy.execute(query, [transaction["category"]])
    for category in supercategories:
        query = plpy.prepare(
          'SELECT * FROM "Balances" WHERE "month_id" = $1 AND "category" = $2',
          ["integer", "text"]
        )
        balances = plpy.execute(query, [month["id"], category["category"]])
        if not balances:
            query = plpy.prepare(
              'INSERT INTO "Balances" ("month_id", "category", "actual") '
              'VALUES ($1, $2, $3)',
              ["integer", "text", "integer"]
            )
            plpy.execute(
              query, [month["id"], category["category"], transaction["amount"]]
            )
        else:
            query = plpy.prepare(
              'UPDATE "Balances" SET "actual" = "actual" + $1 '
              'WHERE "month_id" = $2 AND "category" = $3',
              ["integer", "integer", "text"]
            )
            plpy.execute(
              query, [transaction["amount"], month["id"], category["category"]]
            )
$$ LANGUAGE plpython3u;

CREATE OR REPLACE FUNCTION update_totals()
  RETURNS TRIGGER
AS $$
BEGIN
  CALL update_months(NEW);
  CALL update_balances(NEW);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER update_totals
AFTER INSERT ON "Transactions"
FOR EACH ROW
EXECUTE PROCEDURE update_totals();

-- POPULATE DATABASE

BEGIN;
ALTER TABLE Category DISABLE TRIGGER ALL;
INSERT INTO Category (category, supercategory) VALUES ('root', 'root');
ALTER TABLE Category ENABLE TRIGGER ALL;
COMMIT;

INSERT INTO Category (category, supercategory) VALUES
      ('expense', 'root')
    , ('income', 'root')
    , ('impuestos', 'expense')
    , ('seguros', 'expense')
    , ('vivienda', 'expense')
    , ('suministros', 'expense')
    , ('alimentos', 'expense')
    , ('personal', 'expense')
    , ('credito', 'expense')
    , ('inversiones', 'income')
    , ('sueldo', 'income')
    , ('bonos', 'income')
    , ('intereses', 'income')
    , ('prestamos', 'income')
    , ('deudores', 'income')
    , ('iva', 'impuestos')
    , ('isr', 'impuestos')
    , ('seguro_medico', 'seguros')
    , ('seguro_desempleo', 'seguros')
    , ('pension', 'seguros')
    , ('renta', 'vivienda')
    , ('luz', 'suministros')
    , ('gas', 'suministros')
    , ('internet', 'suministros')
    , ('telefonia', 'suministros')
    , ('transporte', 'suministros')
    , ('entretenimiento', 'suministros')
    , ('classes', 'suministros')
    , ('otros', 'suministros');
