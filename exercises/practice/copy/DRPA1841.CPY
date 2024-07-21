      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM
      *            DATUM      :  06.11.2015
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2015 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRPA1841.CPY  $
      *    $REVISION:   2.0  $
      *
      *    $LOG:   L:/MF/DRPA1841.CPV  $
      *
      *   REV 2.0   NOV 06 2015 15:22:00   T13AE37
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1841                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1834                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * ------------------------------------------------------------- *
      * APA01932!05.11.15! 2.0! NEU                       ! BUCH      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *
      *    DB2-LESE-OZP FÜR DIE TABELLE TBPA0138 PABR
      *    (OHNE SELECT AUF ALLE ATTRIBUTE)
      *
      *    FUNKTION DRPA1841:
      *    LESEN AKTUELLSTE ABRECHNUNGS-NUMMER ZU EINER PABR-VBNR
      *
      *****************************************************************
      *
       01  PI-DRPA1841.
           05  PI-DRPA1841-PABR-VBNR          PIC S9(007) BINARY.
           05  PI-DRPA1841-PGEB-GES           PIC S9(004) BINARY.
           05  PI-DRPA1841-PDPT-ID            PIC S9(015) COMP-3.
      *
       01  PO-DRPA1841.
           05  PO-DRPA1841-RC                 PIC  X(003).
               88 PO-DRPA1841-RC-OK                  VALUE 'OK '.
               88 PO-DRPA1841-RC-NOT-FOUND           VALUE 'NFD'.
      *
           05  PO-DRPA1841-PABR-ABRECH-NR     PIC  X(015).
           05  PO-DRPA1841-PDPT-ID            PIC S9(015) COMP-3.
           05  PO-DRPA1841-PABR-VBNR          PIC S9(007) BINARY.
      *
      ******* ENDE COPY-MEMBER ****************************************

