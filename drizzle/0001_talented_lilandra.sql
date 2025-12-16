CREATE TABLE `rule_history` (
	`id` int AUTO_INCREMENT NOT NULL,
	`ruleId` int NOT NULL,
	`action` enum('CREATE','UPDATE','DELETE','ACTIVATE','DEACTIVATE') NOT NULL,
	`previousState` json,
	`newState` json,
	`changedBy` varchar(64),
	`changeReason` text,
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	CONSTRAINT `rule_history_id` PRIMARY KEY(`id`)
);
--> statement-breakpoint
CREATE TABLE `rules` (
	`id` int AUTO_INCREMENT NOT NULL,
	`name` varchar(100) NOT NULL,
	`description` text,
	`category` enum('VALUE','TEMPORAL','GEOGRAPHIC','MCC','AUTHENTICATION','CVV_PIN','TERMINAL','EMV','CARD','CONTEXT','COMBINED','BRAZIL_SPECIFIC') NOT NULL,
	`classification` enum('APPROVED','SUSPICIOUS','FRAUD') NOT NULL,
	`weight` int NOT NULL DEFAULT 50,
	`conditions` json NOT NULL,
	`logicOperator` enum('AND','OR') NOT NULL DEFAULT 'AND',
	`isActive` boolean NOT NULL DEFAULT true,
	`version` int NOT NULL DEFAULT 1,
	`source` varchar(255),
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	`updatedAt` timestamp NOT NULL DEFAULT (now()) ON UPDATE CURRENT_TIMESTAMP,
	`createdBy` varchar(64),
	CONSTRAINT `rules_id` PRIMARY KEY(`id`),
	CONSTRAINT `rules_name_unique` UNIQUE(`name`)
);
--> statement-breakpoint
CREATE TABLE `transaction_audits` (
	`id` int AUTO_INCREMENT NOT NULL,
	`externalTransactionId` varchar(100) NOT NULL,
	`customerId` varchar(100),
	`merchantId` varchar(100),
	`transactionAmount` int,
	`classification` enum('APPROVED','SUSPICIOUS','FRAUD') NOT NULL,
	`totalScore` int NOT NULL DEFAULT 0,
	`triggeredRules` json,
	`ruleDetails` json,
	`reason` text,
	`originalPayload` json,
	`processingTimeMs` int,
	`createdAt` timestamp NOT NULL DEFAULT (now()),
	CONSTRAINT `transaction_audits_id` PRIMARY KEY(`id`)
);
