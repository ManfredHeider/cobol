      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2667.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2667.CPv  $
      *
      *   Rev 2.6   Oct 02 2017 11:04:14   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.5   Sep 14 2017 14:01:56   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.4   Sep 06 2017 14:14:46   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.3   Aug 08 2013 09:26:44   t13ex52
      *AKI01226: G50 CENTRAL BAP Tarifumstellung 2013
      *
      *   Rev 2.2   Oct 18 2012 10:26:56   T13EX20
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.1   Sep 26 2012 10:13:20   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Aug 22 2012 18:00:42 T13EX52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2667                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2667                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         05.09.2017 / 14:25:43 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2667
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2667.
           05  PI-DRKI2667-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2667-PUMLEISTUNG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2667-PUMPERSONENNR   PIC S9(4) COMP.
             10 PI-DRKI2667-PUMTARIFUMSTELLG   PIC  X(0012).
             10 PI-DRKI2667-PUMVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2667-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2667-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2667-PUMBEITRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2667-PUMGUTSCHRIFTBEF   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2667-PUMRISIKOZUSCHLG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI2667-PUMSELBSTBEHALT   PIC S9(7)V9(2) COMP-3.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2667-NI1.
           05 PI-DRKI2667-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2667-NI7.
           05 PI-DRKI2667-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI2667.
           05  PO-DRKI2667-RC                     PIC  X(003).
               88 PO-DRKI2667-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2667-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

