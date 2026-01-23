package com.rulex.repository;

import com.rulex.entity.TransactionRawStore;
import java.time.LocalDateTime;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface TransactionRawStoreRepository extends JpaRepository<TransactionRawStore, String> {

	/** Delete old raw payloads for retention policy */
	@Modifying
	@Query("DELETE FROM TransactionRawStore r WHERE r.createdAt < :before")
	void deleteOldRawPayloads(@Param("before") LocalDateTime before);
}
