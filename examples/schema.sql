CREATE TABLE IF NOT EXISTS `school` (
    `id` INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT,
    `name` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    `created_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

    UNIQUE KEY `unique_school_name` (`name`)
);

CREATE TABLE IF NOT EXISTS `user` (
    `id` INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT,
    `email` TEXT CHARACTER SET latin1 COLLATE latin1_swedish_ci NOT NULL,
    `state` SMALLINT NOT NULL DEFAULT (0),
    `name` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    `school_id` INTEGER NOT NULL,
    `created_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

    UNIQUE KEY `unique_user_email` (`email`),
    CONSTRAINT `user_school_id_fkey` FOREIGN KEY (`school_id`) REFERENCES `school` (`id`)
);

CREATE TABLE IF NOT EXISTS `group` (
    `id` INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT,
    `name` TEXT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL,
    `created_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `updated_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,

    UNIQUE KEY `unique_group_name` (`name`)
);

CREATE TABLE IF NOT EXISTS `user_group` (
    `user_id` INTEGER NOT NULL,
    `group_id` INTEGER NOT NULL,
    `created_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,

    UNIQUE KEY `unique_user_group_ids` (`user_id`, `group_id`),
    CONSTRAINT `user_group_user_id_fkey` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`),
    CONSTRAINT `user_group_group_id_fkey` FOREIGN KEY (`group_id`) REFERENCES `group` (`id`)
);
