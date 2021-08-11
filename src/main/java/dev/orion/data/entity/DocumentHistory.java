package dev.orion.data.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;

import javax.persistence.Entity;
import javax.persistence.OneToOne;

/**
 * WIP: HISTORY NOT IMPLEMENTED YET
 * **/
@Entity
public class DocumentHistory extends PanacheEntity {
    String content;

    @OneToOne
    ActivityHistory activityHistory;

    @OneToOne
    UserHistory userHistory;
}
