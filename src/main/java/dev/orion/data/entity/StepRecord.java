package dev.orion.data.entity;

import dev.orion.data.entity.Step;
import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Embeddable;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import java.util.ArrayList;
import java.util.List;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class StepRecord extends PanacheEntity {
    @OneToMany
    List<Step> pre = new ArrayList<>();

    @OneToMany
    List<Step> during = new ArrayList<>();

    @OneToMany
    List<Step> post = new ArrayList<>();
}
