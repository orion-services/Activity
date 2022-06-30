package dev.orion.commom.constant;

public enum ActivityStages {
    PRE("pre"), DURING("during"), POS("pos");

    public final String label;

    ActivityStages(String label) {
        this.label = label;
    }
}
