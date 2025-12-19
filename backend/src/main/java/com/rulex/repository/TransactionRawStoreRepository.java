package com.rulex.repository;

import com.rulex.entity.TransactionRawStore;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TransactionRawStoreRepository extends JpaRepository<TransactionRawStore, String> {}
