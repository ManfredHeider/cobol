      *---------------------------------------------------------------*
      *
      * SATZAUFBAU ZUR TABELLE BUZUSTR - TBVB0142
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * VLA-3566!20.10.21!2.0 ! NEU                       ! T13AE30  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       03  DCL-TBVB0142.
           05 WB-BUZUSTR-VS-ZEITRAUM         PIC S9(4)        BINARY.   
           05 WB-BUZUSTR-KS                  PIC S9(4)        BINARY.   
           05 WB-BUZUSTR-BETR                PIC S9(09)       BINARY.   
           05 WB-BUZUSTR-UNTST               PIC S9(09)       BINARY.   
           05 WB-BUZUSTR-AVERM               PIC S9(09)       BINARY.   
           05 WB-BUZUSTR-WECHSEL-DAT         PIC  X(10).
           05 WB-BUZUSTR-DAT-VON             PIC  X(10).
           05 WB-BUZUSTR-DAT-BIS             PIC  X(10).
           05 WB-BUZUSTR-NGEW-VON            PIC  X(10).
           05 WB-BUZUSTR-NGEW-BIS            PIC  X(10).
           05 WB-BUZUSTR-GEW-VON             PIC  X(10).
           05 WB-BUZUSTR-GEW-BIS             PIC  X(10).
           05 WB-BUZUSTR-ERSTELL-DAT         PIC  X(10).
           05 WB-BUZUSTR-EH-DAISY            PIC S9(09)V9(01) COMP-3.   
           05 WB-BUZUSTR-ZUSCHLAG            PIC S9(09)V9(01) COMP-3.
           05 WB-BUZUSTR-ABSCHLAG            PIC S9(09)V9(01) COMP-3.
           05 WB-BUZUSTR-BW-KZ               PIC  X(01).
           05 WB-BUZUSTR-KORR-MON            PIC S9(04)       BINARY.
           05 WB-BUZUSTR-EBENE               PIC S9(04)       BINARY.
