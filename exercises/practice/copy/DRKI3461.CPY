      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3461.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3461.CPv  $
      *
      *   Rev 2.3   Nov 22 2018 14:51:10   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.2   Oct 27 2018 15:28:56   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.1   Sep 21 2018 10:14:18   t13ex78
      *AKI01615: OZPs für G42_BANKPRODUKT
      *
      *   Rev 2.0   Sep 12 2018 08:06:04 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3461                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3461                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2018 / 10:20:24 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3461
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3461.
           05  PI-DRKI3461-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3461-G42PVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3461-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3461-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3461-G42PAKTMONAT   PIC  X(0005).
             10 PI-DRKI3461-G42PBUCHDAT   PIC  X(0008).
             10 PI-DRKI3461-G42PDATEROFFNUNG   PIC  X(0008).
             10 PI-DRKI3461-G42PDATSCHLIESS   PIC  X(0008).
             10 PI-DRKI3461-G42PEXTLIMIT   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3461-G42PHABENZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3461-G42PKONTOART   PIC  X(0002).
             10 PI-DRKI3461-G42PKUMHABENVM   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3461-G42PKUMSOLLVM   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3461-G42PSALDOBETR   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3461-G42PSOLLZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3461-G42PSUMHABENAM   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3461-G42PSUMSOLLAM   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3461-G42PUEBERZZINS   PIC S9(2)V9(4) COMP-3.
             10 PI-DRKI3461-G42PVORMONAT   PIC  X(0005).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3461-NI1.
           05 PI-DRKI3461-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3461-NI7.
           05 PI-DRKI3461-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3461.
           05  PO-DRKI3461-RC                     PIC  X(003).
               88 PO-DRKI3461-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3461-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
 
 

