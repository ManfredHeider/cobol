      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.07.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3353.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3353.CPv  $
      *
      *   Rev 2.1   Jan 12 2021 15:38:58   u130684
      *AKI01775: Neues Ermittlungsprogramm Fondsdepot Bank
      *
      *   Rev 2.0   Jul 02 2018 07:15:08   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3353                                      *
      *    --------------------------------------------------------   *
      *    Cursor Full SELECT ZXKI0488 G79_KUNDEVERTRAG               *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01603!03.07.18!    ! Neuerstellung             ! WHO      *
      * --------!--------!----!---------------------------!----------*
      * AKI01775!05.01.21!    ! KLEINE KORREKTUREN        ! U130684  *
      * --------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3353
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3353.
           05 PI-DRKI3353-UFKT-CODE            PIC  9(002).
              88 PI-DRKI3353-UFKT-FIRST             VALUE 1.
              88 PI-DRKI3353-UFKT-NEXT              VALUE 2.
              88 PI-DRKI3353-UFKT-QUIT              VALUE 3.

           05  PI-DRKI3353-KEY.
               10 PI-DRKI3353-G40MANDANT       PIC S9(9) COMP.

      *
       01  PO-DRKI3353.
           05 PO-DRKI3353-RC                   PIC  X(003).
              88 PO-DRKI3353-RC-OK                  VALUE 'OK '.
              88 PO-DRKI3353-RC-LEER                VALUE 'LER'.
              88 PO-DRKI3353-RC-EOF                 VALUE 'EOF'.
      *
           05 PO-DRKI3353-DATEN.
             10 PO-DRKI3353-G40VERTRAGSNR   PIC  X(0015).
             10 PO-DRKI3353-G40MANDANT   PIC S9(9) COMP.
             10 PO-DRKI3353-G40ANACHLASSKZ   PIC  X(0001).
             10 PO-DRKI3353-G40BANK   PIC  X(0020).
             10 PO-DRKI3353-G40BIC   PIC  X(0011).
             10 PO-DRKI3353-G40IBAN   PIC  X(0034).
             10 PO-DRKI3353-G40INTVERTNR   PIC S9(9) COMP.
             10 PO-DRKI3353-G40KONTOART   PIC  X(0002).
             10 PO-DRKI3353-G40KUNDENDATEN.
                49 PO-DRKI3353-G40KUNDENDATEN-LEN PIC S9(4) COMP.
                49 PO-DRKI3353-G40KNDNDTN-TEXT PIC X(322).
             10 PO-DRKI3353-G40RISIKOKLASSE   PIC  X(0001).
             10 PO-DRKI3353-G40VBNR   PIC S9(9) COMP.
             10 PO-DRKI3353-G40VERTABLAFFKZ   PIC  X(0001).
             10 PO-DRKI3353-G40VERTAENDDAT   PIC  X(0008).
             10 PO-DRKI3353-G40VERTBEGINNKZ   PIC  X(0001).
             10 PO-DRKI3353-G40VRTRGABLAUF   PIC  X(0008).
             10 PO-DRKI3353-G40VRTRGBEGHIST   PIC  X(0008).
             10 PO-DRKI3353-G40VRTRGBEGINN   PIC  X(0008).
      *
      ******* ENDE COPY-MEMBER ***************************************



