      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3349.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3349.CPv  $
      *
      *   Rev 2.1   Jan 12 2021 15:38:36   u130684
      *AKI01775: Neues Ermittlungsprogramm Fondsdepot Bank
      *
      *   Rev 2.0   Apr 17 2020 09:49:40 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3349                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3349                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.12.2020 / 09:56:59 / U130684
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3349
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3349.
           05  PI-DRKI3349-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3349-G40VERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3349-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3349-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3349-G40ANACHLASSKZ   PIC  X(0001).
             10 PI-DRKI3349-G40BANK   PIC  X(0020).
             10 PI-DRKI3349-G40BIC   PIC  X(0011).
             10 PI-DRKI3349-G40IBAN   PIC  X(0034).
             10 PI-DRKI3349-G40INTVERTNR   PIC S9(9) COMP.
             10 PI-DRKI3349-G40KONTOART   PIC  X(0002).
             10 PI-DRKI3349-G40KUNDENDATEN.
                49 PI-DRKI3349-G40KUNDENDATEN-LEN PIC S9(4) COMP.
                49 PI-DRKI3349-G40KNDNDTN-TEXT PIC X(322).
             10 PI-DRKI3349-G40MANDANT   PIC S9(9) COMP.
             10 PI-DRKI3349-G40RISIKOKLASSE   PIC  X(0001).
             10 PI-DRKI3349-G40VBNR   PIC S9(9) COMP.
             10 PI-DRKI3349-G40VERTABLAFFKZ   PIC  X(0001).
             10 PI-DRKI3349-G40VERTAENDDAT   PIC  X(0008).
             10 PI-DRKI3349-G40VERTBEGINNKZ   PIC  X(0001).
             10 PI-DRKI3349-G40VRTRGABLAUF   PIC  X(0008).
             10 PI-DRKI3349-G40VRTRGBEGHIST   PIC  X(0008).
             10 PI-DRKI3349-G40VRTRGBEGINN   PIC  X(0008).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3349-NI1.
           05 PI-DRKI3349-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3349-NI7.
           05 PI-DRKI3349-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3349.
           05  PO-DRKI3349-RC                     PIC  X(003).
               88 PO-DRKI3349-RC-OK               VALUE 'OK '.
               88 PO-DRKI3349-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

