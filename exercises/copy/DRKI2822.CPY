      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2822.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2822.CPv  $
      *
      *   Rev 2.7   Oct 01 2018 09:37:10   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.6   Sep 27 2018 16:58:22   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.5   Jan 25 2018 06:52:32   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.3   Nov 06 2015 12:27:06   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens   OZP ZTKI0466
      *
      *   Rev 2.2   Apr 16 2014 17:12:02   t13ex19
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 23 2014 15:35:52   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Dec 10 2013 11:20:56   i13az40
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2822                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2822                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.09.2018 / 10:05:57 / T13AE52
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
      *    FÜR DIE FUNKTION DRKI2822
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2822.
           05  PI-DRKI2822-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2822-KKALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2822-KKAPRODUKTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2822-KKAVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2822-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2822-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2822-KKAABRECHKTONR   PIC  X(0010).
             10 PI-DRKI2822-KKAABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2822-KKABLZABRECHKTO   PIC  X(0008).
             10 PI-DRKI2822-KKAEIGENVERMKZ   PIC  X(0001).
             10 PI-DRKI2822-KKAFILNRABRECHKTO   PIC S9(4) COMP.
             10 PI-DRKI2822-KKAHAUPTKARTENNR   PIC  X(0016).
             10 PI-DRKI2822-KKAKARTENART   PIC  X(0003).
             10 PI-DRKI2822-KKAKARTENNR   PIC  X(0016).
             10 PI-DRKI2822-KKALIMIT   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2822-KKAMAHNSTATUS   PIC  X(0001).
             10 PI-DRKI2822-KKAOEFFNUNGSDAT   PIC  X(010).
             10 PI-DRKI2822-KKAPIN   PIC S9(9) COMP.
             10 PI-DRKI2822-KKAPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2822-KKAPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2822-KKAVNRHAUPTKARTE   PIC  X(0015).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2822-NI1.
           05 PI-DRKI2822-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2822-NI7.
           05 PI-DRKI2822-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI2822.
           05  PO-DRKI2822-RC                     PIC  X(003).
               88 PO-DRKI2822-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2822-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


