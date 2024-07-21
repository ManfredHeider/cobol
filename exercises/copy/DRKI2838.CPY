      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2838.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2838.CPv  $
      *
      *   Rev 2.6   Nov 21 2018 09:42:00   t13ae52
      *FKI01579: Fehlerbereinigung VBI-Schnittstelle
      *
      *   Rev 2.5   Oct 01 2018 09:36:30   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.4   Sep 27 2018 16:57:48   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.3   Nov 06 2015 12:27:34   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens   OZP ZTKI0466
      *
      *   Rev 2.2   Jan 27 2014 08:45:50   t13ex77
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.1   Jan 22 2014 12:18:32   T13Ex77
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Jan 14 2014 10:44:24   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2838                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2838                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI01269 !14.01.14!1.0 ! Neuerstellung             ! abn      *
      * -------------------------------------------------------------*
061115* AKI01385!06.11.15!2.0 ! PROSIONS KZ einbauen      ! DDY      *
      * -------------------------------------------------------------*
      *AKI01632 !01.10.18!2.6 ! Hinzufügen neues Attribut !Schumacher*
      *         !        !    ! KKALIMIT                  !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2838
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2838.
           05 PI-DRKI2838-PROGRAMM                 PIC  X(008).
           05 PI-DRKI2838-UFKT-CODE                PIC  9(002).
              88 PI-DRKI2838-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI2838-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI2838-UFKT-QUIT                  VALUE 3.
           05 PI-DRKI2838-DATEN.
              10 PI-DRKI2838-KKAVERTRAGSID         PIC X(13).
      *
       01  PO-DRKI2838.
           05  PO-DRKI2838-RC                      PIC  X(003).
               88 PO-DRKI2838-RC-OK                     VALUE 'OK '.
               88 PO-DRKI2838-RC-EOF                    VALUE 'EOF'.
               88 PO-DRKI2838-RC-LEER                   VALUE 'LER'.
      *
           05  PO-DRKI2838-KEY.
             10 PO-DRKI2838-KKALFDNR               PIC S9(4) COMP.
             10 PO-DRKI2838-KKAPRODUKTLFDNR        PIC S9(4) COMP.
             10 PO-DRKI2838-KKAVERTRAGSID          PIC  X(0013).
      *
           05  PO-DRKI2838-DATEN.
             10 PO-DRKI2838-KKAABRECHKTONR         PIC  X(0010).
             10 PO-DRKI2838-KKAABSCHLUSSVBNR       PIC S9(9) COMP.
             10 PO-DRKI2838-KKABLZABRECHKTO        PIC  X(0008).
             10 PO-DRKI2838-KKAEIGENVERMKZ         PIC  X(0001).
             10 PO-DRKI2838-KKAFILNRABRECHKTO      PIC S9(4) COMP.
             10 PO-DRKI2838-KKAHAUPTKARTENNR       PIC  X(0016).
             10 PO-DRKI2838-KKAKARTENART           PIC  X(0003).
             10 PO-DRKI2838-KKAKARTENNR            PIC  X(0016).
             10 PO-DRKI2838-KKALIMIT               PIC S9(6)V9(2)
                                                       COMP-3.
             10 PO-DRKI2838-KKAMAHNSTATUS          PIC  X(0001).
             10 PO-DRKI2838-KKAOEFFNUNGSDAT        PIC  X(010).
             10 PO-DRKI2838-KKAPIN                 PIC S9(9) COMP.
             10 PO-DRKI2838-KKAPRODUKTID           PIC S9(4) COMP.
061115       10 PO-DRKI2838-KKAPROVISIONKZ         PIC  X(0001).
             10 PO-DRKI2838-KKAVNRHAUPTKARTE       PIC  X(0015).
      *
      ******* ENDE COPY-MEMBER ***************************************
 
 
 

