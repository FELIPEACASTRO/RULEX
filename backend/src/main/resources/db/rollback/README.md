# Database Rollback Scripts

## ⚠️ WARNING

These scripts are for **EMERGENCY USE ONLY**. They will:
- Drop tables and indexes
- **PERMANENTLY DELETE DATA**
- Potentially break the application

## Usage

Rollback scripts must be run **manually** and in **reverse order** (R7 → R1).

### Before Running

1. **BACKUP YOUR DATABASE**
   ```bash
   pg_dump -U postgres rulex_db > backup_$(date +%Y%m%d_%H%M%S).sql
   ```

2. Verify you have the correct backup
3. Notify all stakeholders
4. Plan for downtime

### Running a Rollback

```bash
# Connect to database
psql -U postgres -d rulex_db

# Run rollback script (example: rollback V7)
\i R7__undo_v31_exec_log_dedup.sql

# Verify the rollback
\dt  -- list tables
\di  -- list indexes
```

### Rollback Order

To rollback to a specific version, run scripts in reverse order:

| Target Version | Scripts to Run |
|----------------|----------------|
| V6 | R7 |
| V5 | R7, R6 |
| V4 | R7, R6, R5 |
| V3 | R7, R6, R5, R4 |
| V2 | R7, R6, R5, R4, R3 |
| V1 | R7, R6, R5, R4, R3, R2 |
| Clean | R7, R6, R5, R4, R3, R2, R1 |

### After Rollback

1. Update Flyway schema history:
   ```sql
   DELETE FROM flyway_schema_history WHERE version > 'X';
   ```
   (Replace X with target version)

2. Restart the application
3. Verify functionality
4. Monitor logs for errors

## Script Descriptions

| Script | Undoes | Risk Level |
|--------|--------|------------|
| R7 | Dedup index changes | Low |
| R6 | Field dictionary & exec log | Medium |
| R5 | Raw as received column | Low |
| R4 | Payload hash idempotency | Medium |
| R3 | Workflow length extension | Low |
| R2 | Core schema tables | **HIGH** |
| R1 | Initial schema | **CRITICAL** |

## Contact

For assistance with rollbacks, contact:
- DevOps Team
- Database Administrator
- On-call Engineer
