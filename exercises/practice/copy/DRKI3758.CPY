      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.01.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3758.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3758.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 12:55:00   t13ex78
      *AKI01731: Standard OZP G66_PRIVATKREDIT
      *
      *   Rev 2.0   Jan 09 2020 10:52:08   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3758                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3758                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         21.01.2020 / 11:45:21 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3758
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3758.
           05  PI-DRKI3758-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3758-G66PVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3758-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3758-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3758-G66PABSCHLVBNR   PIC S9(9) COMP.
             10 PI-DRKI3758-G66PBEGINNDATUM   PIC  X(0008).
             10 PI-DRKI3758-G66PBICBELAST   PIC  X(0011).
             10 PI-DRKI3758-G66PDARLBETR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3758-G66PENDEDATUM   PIC  X(0008).
             10 PI-DRKI3758-G66PFAELLIGDAT   PIC  X(0008).
             10 PI-DRKI3758-G66PIBANBELAST   PIC  X(0034).
             10 PI-DRKI3758-G66PIBANVERTRAG   PIC  X(0034).
             10 PI-DRKI3758-G66PKREDITART   PIC  X(0006).
             10 PI-DRKI3758-G66PKREDITAUSDAT   PIC  X(0008).
             10 PI-DRKI3758-G66PKREDITZUSDAT   PIC  X(0008).
             10 PI-DRKI3758-G66PMANDANT   PIC S9(9) COMP.
             10 PI-DRKI3758-G66PMANDATSREF   PIC  X(0035).
             10 PI-DRKI3758-G66PRATEBETR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3758-G66PRESTLAUFZEIT   PIC  X(0003).
             10 PI-DRKI3758-G66PSALDOBETR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3758-G66PSEPANACHNAME   PIC  X(0030).
             10 PI-DRKI3758-G66PSEPAVORNAME   PIC  X(0030).
             10 PI-DRKI3758-G66PSICHERHEIT   PIC  X(0001).
             10 PI-DRKI3758-G66PZINSSATZ   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3758-G66PZUSTAND   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3758-NI1.
           05 PI-DRKI3758-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3758-NI7.
           05 PI-DRKI3758-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3758.
           05  PO-DRKI3758-RC                     PIC  X(003).
               88 PO-DRKI3758-RC-OK               VALUE 'OK '.
               88 PO-DRKI3758-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

