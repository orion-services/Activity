package dev.orion.commom.constant;

public enum ActivityStages {
    PRE("pre"), DURING("during"), AFTER("after");

    public final String label;

    private ActivityStages(String label) {
        this.label = label;
    }
}
