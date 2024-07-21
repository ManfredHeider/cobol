      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.09.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2425.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2425.CPv  $
      *
      *   Rev 2.4   Oct 11 2019 10:02:10   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer 
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.3   May 06 2019 13:48:02   t13ex27
      *AKI01683: Capital Bank - Neuer WAG-Bogen (WpHG) nicht immer
      *erforderlich, sofern bereits ein aktueller vorliegt - Erweit
      *ands-DTA Depot
      *
      *   Rev 2.2   Sep 29 2010 07:46:46   t13ex15
      *AKI00966: DVAG Österreich: Kreditsparkonto Aktualisierung
      *
      *   Rev 2.1   Sep 23 2010 12:21:50   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-OZP GUT
      *HABENPRODUKT
      *
      *   Rev 2.0   Sep 22 2010 08:22:06   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2425                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2425                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         24.04.2019 / 07:33:36 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2425
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2425.
           05  PI-DRKI2425-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2425-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2425-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2425-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2425-N-GHPDTLNM-KNR      PIC S9(4) COMP.
             15 PI-DRKI2425-N-GHPDTLNM-KTY      PIC S9(4) COMP.
             15 PI-DRKI2425-N-GHPFFNNGSDTM      PIC S9(4) COMP.
             15 PI-DRKI2425-NI-GHPRISIKOKLASSE  PIC S9(4) COMP.
             15 PI-DRKI2425-N-GHPSCHLSSDTM      PIC S9(4) COMP.
             15 PI-DRKI2425-NI-GHPWPHGDAT       PIC S9(4) COMP.
             15 PI-DRKI2425-NI-GHPZINSSATZ      PIC S9(4) COMP.
END   *

           05  PI-DRKI2425-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2425-GHPDETAILNAM-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2425-GHPDETAILNAM-KTY   PIC S9(4) COMP.
             10 PI-DRKI2425-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI2425-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2425-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI2425-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI2425-GHPWPHGDAT   PIC  X(010).
             10 PI-DRKI2425-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2425-NI1.
           05 PI-DRKI2425-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 7.
       01  PI-DRKI2425-NI7.
           05 PI-DRKI2425-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 7.

      *
       01  PO-DRKI2425.
           05  PO-DRKI2425-RC                     PIC  X(003).
               88 PO-DRKI2425-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2425-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


