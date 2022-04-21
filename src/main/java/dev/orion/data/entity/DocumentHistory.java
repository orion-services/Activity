package dev.orion.data.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;

import javax.persistence.Entity;
import javax.persistence.ManyToOne;

/**
 * WIP: HISTORY NOT IMPLEMENTED YET
 * **/
@Entity
public class DocumentHistory extends PanacheEntity {
    String content;

    @ManyToOne
    Document document;

    @ManyToOne
    ActivityHistory activityHistory;

    @ManyToOne
    UserHistory userHistory;
}
