      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.12.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2821.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2821.CPv  $
      *
      *   Rev 2.4   Oct 01 2018 09:36:50   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.3   Sep 27 2018 16:58:04   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.2   Apr 16 2014 17:11:56   t13ex19
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 21 2014 12:05:02   i13az40
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Dec 10 2013 16:12:10 I13AZ40
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2821                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2821                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         27.09.2018 / 10:05:55 / T13AE52
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
      *    FÜR DIE FUNKTION DRKI2821
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2821.
           05  PI-DRKI2821-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2821-KKALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2821-KKAPRODUKTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2821-KKAVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2821.
           05  PO-DRKI2821-RC                     PIC  X(003).
               88 PO-DRKI2821-RC-OK               VALUE 'OK '.
               88 PO-DRKI2821-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *

           05  PO-DRKI2821-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2821-KKAABRECHKTONR   PIC  X(0010).
             10 PO-DRKI2821-KKAABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI2821-KKABLZABRECHKTO   PIC  X(0008).
             10 PO-DRKI2821-KKAEIGENVERMKZ   PIC  X(0001).
             10 PO-DRKI2821-KKAFILNRABRECHKTO   PIC S9(4) COMP.
             10 PO-DRKI2821-KKAHAUPTKARTENNR   PIC  X(0016).
             10 PO-DRKI2821-KKAKARTENART   PIC  X(0003).
             10 PO-DRKI2821-KKAKARTENNR   PIC  X(0016).
             10 PO-DRKI2821-KKAMAHNSTATUS   PIC  X(0001).
             10 PO-DRKI2821-KKAOEFFNUNGSDAT   PIC  X(010).
             10 PO-DRKI2821-KKAPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2821-KKAVNRHAUPTKARTE   PIC  X(0015).
             10 PO-DRKI2821-LETZTEAENDERUNG   PIC  X(0026).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************


