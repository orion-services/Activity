package dev.orion.commom.constant;

public enum ActivityStage {
    PRE("pre"), DURING("during"), POS("pos");

    public final String label;

    ActivityStage(String label) {
        this.label = label;
    }
}
