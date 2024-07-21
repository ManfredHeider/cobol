      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3636.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3636.CPv  $
      *
      *   Rev 2.2   Jun 07 2019 08:20:24   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.1   Mar 19 2019 12:42:54   t13ex78
      *AKI01669: OZP G67_KDETICKETS
      *
      *   Rev 2.0   Mar 18 2019 11:12:20 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3636                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3636                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         18.03.2019 / 11:12:20 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3636
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3636.
           05  PI-DRKI3636-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3636-G67TLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3636-G67TVERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3636-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3636-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3636-G67TANREDE   PIC  X(0001).
             10 PI-DRKI3636-G67TGEBDAT   PIC  X(0010).
             10 PI-DRKI3636-G67TLAND   PIC  X(0003).
             10 PI-DRKI3636-G67TNACHNAME   PIC  X(0030).
             10 PI-DRKI3636-G67TORT   PIC  X(0030).
             10 PI-DRKI3636-G67TPLZ   PIC  X(0009).
             10 PI-DRKI3636-G67TSTRHNR   PIC  X(0040).
             10 PI-DRKI3636-G67TTICKETID   PIC S9(18)V COMP-3.
             10 PI-DRKI3636-G67TVORNAME   PIC  X(0030).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3636-NI1.
           05 PI-DRKI3636-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3636-NI7.
           05 PI-DRKI3636-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3636.
           05  PO-DRKI3636-RC                     PIC  X(003).
               88 PO-DRKI3636-RC-OK               VALUE 'OK '.
               88 PO-DRKI3636-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


