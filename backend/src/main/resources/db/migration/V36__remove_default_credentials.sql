-- V36__remove_default_credentials.sql
-- Remove usuário admin com senha placeholder criado em V1__init.sql
-- Este usuário foi criado apenas para desenvolvimento inicial
-- Em produção, usuários devem ser criados via API ou script de deploy com senhas seguras

-- Remove vínculos de roles primeiro (FK constraint)
DELETE FROM user_roles 
WHERE user_id IN (
    SELECT id FROM users WHERE password_hash LIKE '%placeholder%'
);

-- Remove usuários com senha placeholder
DELETE FROM users 
WHERE password_hash LIKE '%placeholder%';

-- Log da operação
DO $$
BEGIN
    RAISE NOTICE 'Usuários com senha placeholder removidos. Configure credenciais via variáveis de ambiente.';
END $$;
