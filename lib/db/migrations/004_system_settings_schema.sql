BEGIN;

CREATE TABLE IF NOT EXISTS system_settings (
  id INTEGER PRIMARY KEY DEFAULT 1,
  settings JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  CONSTRAINT single_row CHECK (id = 1)
);

INSERT INTO system_settings (id, settings)
VALUES (1, '{
  "quickbaseRealm": "kin.quickbase.com",
  "quickbaseToken": "",
  "milestoneSLA": {
    "survey": 7,
    "design": 10,
    "permit": 21,
    "nem": 14,
    "install": 7,
    "inspection": 5,
    "pto": 10
  },
  "warningThreshold": 75,
  "criticalThreshold": 100,
  "holdReasons": ["Finance Hold", "Roof Hold", "Customer Hold", "Permit Hold", "HOA Hold"],
  "dateFormat": "MM/DD/YYYY",
  "timezone": "America/Phoenix",
  "sessionTimeout": 60
}'::jsonb)
ON CONFLICT (id) DO NOTHING;

CREATE INDEX IF NOT EXISTS idx_system_settings_jsonb 
  ON system_settings USING gin(settings);

COMMIT;


