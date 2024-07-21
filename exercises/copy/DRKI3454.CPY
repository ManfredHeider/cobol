      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3454.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3454.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:42:34   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.2   Oct 27 2018 15:25:32   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.1   Sep 19 2018 11:47:42   t13ex78
      *AKI01615: OZPs für G42_FSA
      *
      *   Rev 2.0   Sep 12 2018 07:56:52 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3454                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3454                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 10:00:03 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3454
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3454.
           05  PI-DRKI3454-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3454-G42FVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3454-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3454-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3454-G42FAUFTKONF1   PIC S9(3)V COMP-3.
             10 PI-DRKI3454-G42FAUFTKONF2   PIC S9(3)V COMP-3.
             10 PI-DRKI3454-G42FAUSLQSTEUER   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3454-G42FFBETRVERBR   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3454-G42FFSAID   PIC  X(0008).
             10 PI-DRKI3454-G42FGEMFBETRAG   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3454-G42FGUELTIGAB   PIC  X(0008).
             10 PI-DRKI3454-G42FGUELTIGBIS   PIC  X(0008).
             10 PI-DRKI3454-G42FKISTEBEHALT   PIC  X(0008).
             10 PI-DRKI3454-G42FKISTSATZ1   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3454-G42FKISTSATZ2   PIC S9(2)V9(2) COMP-3.
             10 PI-DRKI3454-G42FKONFESSION1   PIC  X(0002).
             10 PI-DRKI3454-G42FKONFESSION2   PIC  X(0002).
             10 PI-DRKI3454-G42FKZSTAUSL   PIC  X(0001).
             10 PI-DRKI3454-G42FKZVERBAKTIE   PIC  X(0001).
             10 PI-DRKI3454-G42FKZVERBSONST   PIC  X(0001).
             10 PI-DRKI3454-G42FNVGUELTIGBIS   PIC  X(0008).
             10 PI-DRKI3454-G42FSTUECKZINS   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI3454-G42FSUMERTRAG   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3454-G42FSUMKEST   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3454-G42FSUMKIST   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3454-G42FSUMSOLI   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3454-G42FVERBVORJ   PIC S9(5)V9(2) COMP-3.
             10 PI-DRKI3454-G42FVERLUSTAKTI   PIC S9(7)V9(2) COMP-3.
             10 PI-DRKI3454-G42FVERLUSTSON   PIC S9(7)V9(2) COMP-3.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3454-NI1.
           05 PI-DRKI3454-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3454-NI7.
           05 PI-DRKI3454-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3454.
           05  PO-DRKI3454-RC                     PIC  X(003).
               88 PO-DRKI3454-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3454-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

