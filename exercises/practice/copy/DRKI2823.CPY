      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2823.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2823.CPv  $
      *
      *   Rev 2.7   Oct 01 2018 09:37:28   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.6   Sep 27 2018 16:58:38   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.5   Jan 25 2018 06:52:46   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.3   Nov 06 2015 12:27:10   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens   OZP ZTKI0466
      *
      *   Rev 2.2   Apr 16 2014 17:12:06   t13ex19
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 14 2014 13:15:40   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Dec 09 2013 16:59:40   i13az40
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2823                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2823                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.09.2018 / 10:05:59 / T13AE52
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
      *    FÜR DIE FUNKTION DRKI2823
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2823.
           05  PI-DRKI2823-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2823-KKALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2823-KKAPRODUKTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2823-KKAVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2823-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2823-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2823-KKAABRECHKTONR   PIC  X(0010).
             10 PI-DRKI2823-KKAABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2823-KKABLZABRECHKTO   PIC  X(0008).
             10 PI-DRKI2823-KKAEIGENVERMKZ   PIC  X(0001).
             10 PI-DRKI2823-KKAFILNRABRECHKTO   PIC S9(4) COMP.
             10 PI-DRKI2823-KKAHAUPTKARTENNR   PIC  X(0016).
             10 PI-DRKI2823-KKAKARTENART   PIC  X(0003).
             10 PI-DRKI2823-KKAKARTENNR   PIC  X(0016).
             10 PI-DRKI2823-KKALIMIT   PIC S9(6)V9(2) COMP-3.
             10 PI-DRKI2823-KKAMAHNSTATUS   PIC  X(0001).
             10 PI-DRKI2823-KKAOEFFNUNGSDAT   PIC  X(010).
             10 PI-DRKI2823-KKAPIN   PIC S9(9) COMP.
             10 PI-DRKI2823-KKAPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2823-KKAPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2823-KKAVNRHAUPTKARTE   PIC  X(0015).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2823-NI1.
           05 PI-DRKI2823-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2823-NI7.
           05 PI-DRKI2823-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2823.
           05  PO-DRKI2823-RC                     PIC  X(003).
               88 PO-DRKI2823-RC-OK               VALUE 'OK '.
               88 PO-DRKI2823-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************


