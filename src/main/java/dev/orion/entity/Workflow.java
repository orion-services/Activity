package dev.orion.entity;

import io.quarkus.hibernate.orm.panache.PanacheEntity;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import java.util.HashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class Workflow extends PanacheEntity {
    @Column(nullable = false)
    private String name;

    @Column(length = 500)
    private String description;

    @OneToMany(cascade = CascadeType.ALL)
    private Set<Stage> stages = new HashSet<>();

    public void addStepStage(Stage stage) {
        stages.add(stage);
    }
    public static Optional<Workflow> findByName(String name) {
        return Workflow.find("name", name).firstResultOptional();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Workflow workflow = (Workflow) o;
        return name.equals(workflow.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
