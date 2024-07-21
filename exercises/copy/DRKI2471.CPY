      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.10.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2471.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2471.CPv  $
      *
      *   Rev 2.4   Dec 19 2016 10:59:52   t13ex27
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.3   Nov 16 2016 08:48:50   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.2   Oct 27 2016 11:43:38   t13ex73
      *AKI01378: Kundenaktualisierung - Kundennummer aus dem Antrag
      *
      *   Rev 2.1   Sep 18 2012 13:58:06   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-OZP Kunde
      *
      *   Rev 2.0   Oct 17 2011 18:32:24 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2471                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2471                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.10.2016 / 15:01:48 / T13EX73
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
      *    FÜR DIE FUNKTION DRKI2471
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2471.
           05  PI-DRKI2471-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2471-KNDPERSONENID   PIC  X(0013).
END   *
 
           05  PI-DRKI2471-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2471-NI-KNDANTRAGSKNR   PIC S9(4) COMP.
             15 PI-DRKI2471-NI-KNDNUMMER       PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2471-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2471-KNDALTKUNDE   PIC  X(010).
             10 PI-DRKI2471-KNDANREDEID   PIC S9(4) COMP.
             10 PI-DRKI2471-KNDANTRAGSKNR   PIC S9(9) COMP.
             10 PI-DRKI2471-KNDANZNAME   PIC  X(0030).
             10 PI-DRKI2471-KNDANZVORNAME   PIC  X(0030).
             10 PI-DRKI2471-KNDGEANDERTPGM   PIC  X(0008).
             10 PI-DRKI2471-KNDGEBGRUENDDAT   PIC  X(010).
             10 PI-DRKI2471-KNDGESCHLECHT   PIC  X(0001).
             10 PI-DRKI2471-KNDMANDANTNR   PIC S9(9) COMP.
             10 PI-DRKI2471-KNDNEUKUNDE   PIC  X(010).
             10 PI-DRKI2471-KNDNUMMER   PIC S9(9) COMP.
             10 PI-DRKI2471-KNDSTATUSID   PIC  X(0001).
             10 PI-DRKI2471-KNDSUCHNAME   PIC  X(0030).
             10 PI-DRKI2471-KNDSUCHVORNAME   PIC  X(0030).
             10 PI-DRKI2471-KNDWIK   PIC  X(010).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2471-NI1.
           05 PI-DRKI2471-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 2.
       01  PI-DRKI2471-NI7.
           05 PI-DRKI2471-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 2.
 
      *
       01  PO-DRKI2471.
           05  PO-DRKI2471-RC                     PIC  X(003).
               88 PO-DRKI2471-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2471-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 


