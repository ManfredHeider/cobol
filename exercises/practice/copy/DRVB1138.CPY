      *****************************************************************
      *    COPY-MEMBER: DRVB1138                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0400                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: OBER                                             *
      *    DATUM:    12.05.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01400!12.05.16!2.1 ! NEU                       ! OBER      *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1138 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1138.
          05 PI-DRVB1138-DATEN.
             10 PI-DRVB1138-ST-NR              PIC S9(09)       BINARY.
             10 PI-DRVB1138-STAB-BUCH-DAT      PIC  X(10).
             10 PI-DRVB1138-STAB-BEZUGS-JAHR   PIC S9(04)       BINARY.
             10 PI-DRVB1138-STAB-LFD           PIC S9(09)       BINARY.
 
       01 PO-DRVB1138.
         03 PO-DRVB1138-RC                     PIC  X(03).
             88 PO-DRVB1138-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1138-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1138-DATEN.
          05 PO-DRVB1138-NI-VIVB0400.
             10 NI-DRVB1138-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BUCH-DAT      PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BEZUGS-JAHR   PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-STEUER-JAHR   PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-DAT-HERKUNFT  PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-LFD           PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BEZUG-LFD     PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-ABR-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1138-KTY-VERARB-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1138-KNR-VERARB-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-SHKZ          PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BUCH-TEXT     PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BUCH-TEXT2    PIC S9(04)       BINARY.
             10 NI-DRVB1138-KTY-USTAT-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1138-KNR-USTAT-TYP      PIC S9(04)       BINARY.
             10 NI-DRVB1138-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1138-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-KONTO-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BELEGART      PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BELEG-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-TURNUS        PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BELEG-DAT     PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-KOSTEN-STELLE PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-KONTO-KU      PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BETRAG-KU     PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-KONTO-U       PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-BETRAG-U      PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-UST-ANTEIL-U  PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-STS           PIC S9(04)       BINARY.
             10 NI-DRVB1138-STAB-UST-BERECHNET PIC S9(04)       BINARY.
          05 PO-DRVB1138-DCL-VIVB0400.
             10 PO-DRVB1138-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1138-STAB-BUCH-DAT      PIC  X(10).
             10 PO-DRVB1138-STAB-BEZUGS-JAHR   PIC S9(04)       BINARY.
             10 PO-DRVB1138-STAB-STEUER-JAHR   PIC S9(04)       BINARY.
             10 PO-DRVB1138-STAB-DAT-HERKUNFT  PIC  X(01).
             10 PO-DRVB1138-STAB-LFD           PIC S9(09)       BINARY.
             10 PO-DRVB1138-STAB-BEZUG-LFD     PIC S9(09)       BINARY.
             10 PO-DRVB1138-STAB-ABR-NR        PIC  X(15).
             10 PO-DRVB1138-KTY-VERARB-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1138-KNR-VERARB-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1138-STAB-SHKZ          PIC  X(01).
             10 PO-DRVB1138-STAB-BUCH-TEXT     PIC  X(40).
             10 PO-DRVB1138-STAB-BUCH-TEXT2    PIC  X(52).
             10 PO-DRVB1138-KTY-USTAT-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1138-KNR-USTAT-TYP      PIC S9(04)       BINARY.
             10 PO-DRVB1138-KTY-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1138-KNR-WAEHRUNG       PIC S9(04)       BINARY.
             10 PO-DRVB1138-STAB-KONTO-TYP     PIC  X(01).
             10 PO-DRVB1138-STAB-BELEGART      PIC  X(03).
             10 PO-DRVB1138-STAB-BELEG-NR      PIC  X(07).
             10 PO-DRVB1138-STAB-TURNUS        PIC  X(06).
             10 PO-DRVB1138-STAB-BELEG-DAT     PIC  X(08).
             10 PO-DRVB1138-STAB-KOSTEN-STELLE PIC  X(08).
             10 PO-DRVB1138-STAB-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1138-STAB-KONTO-KU      PIC  X(12).
             10 PO-DRVB1138-STAB-BETRAG-KU     PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1138-STAB-KONTO-U       PIC  X(12).
             10 PO-DRVB1138-STAB-BETRAG-U      PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1138-STAB-UST-ANTEIL-U  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1138-STAB-STS           PIC S9(04)       BINARY.
             10 PO-DRVB1138-STAB-UST-BERECHNET PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1138   --------------------------*
      *---------------------------------------------------------------*

