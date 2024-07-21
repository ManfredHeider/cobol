      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3102.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3102.CPv  $
      *
      *   Rev 2.3   Apr 17 2018 10:13:52   t13ac17
      *AKI01577: FW - G51/BKKLinde
      *
      *   Rev 2.2   Dec 19 2016 13:10:02   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.1   Dec 07 2016 10:51:28   t13ac20
      *AKI01466: Standard OZP
      *
      *   Rev 2.0   Sep 21 2016 11:59:02   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3102                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3102                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01466!21.09.16!2.1 ! NEUERSTELLUNG             ! SWO      *
      * AKI01466!19.12.16!2.2 ! FELDER ENTFERNT:          ! SWO      *
      *         !        !    !   - G51VRAHMENVERT        !          *
      *         !        !    !   - G51VABRECHDAT         !          *
260318* AKI01577!26.03.18!2.3 ! Mandant als Eingabefeld   ! GWA      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3102
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: LIST
      *
      ****************************************************************
      *
       01  PI-DRKI3102.
           05 PI-DRKI3102-UFKT-CODE               PIC  9(002).
              88 PI-DRKI3102-UFKT-FIRST                 VALUE 1.
              88 PI-DRKI3102-UFKT-NEXT                  VALUE 2.
              88 PI-DRKI3102-UFKT-QUIT                  VALUE 3.
      *
260318     05  PI-DRKI3102-KEY.
260318         10 PI-DRKI3102-G51VMANDANT         PIC S9(009) BINARY.
      *
       01  PO-DRKI3102.
           05 PO-DRKI3102-RC                      PIC  X(003).
              88 PO-DRKI3102-RC-OK                       VALUE 'OK '.
              88 PO-DRKI3102-RC-LEER                     VALUE 'LER'.
              88 PO-DRKI3102-RC-EOF                      VALUE 'EOF'.
      *
           05 PO-DRKI3102-DATEN.
              10 PO-DRKI3102-G51VVERTRAGSNR       PIC  X(015).
              10 PO-DRKI3102-G51VSTATUS           PIC  X(001).
              10 PO-DRKI3102-G51VMAKLERDAT        PIC  X(008).
              10 PO-DRKI3102-G51VANTRAGSPIN       PIC S9(9) BINARY.
              10 PO-DRKI3102-G51VVERSART          PIC  X(001).
              10 PO-DRKI3102-G51VMANDANT          PIC S9(9) BINARY.
              10 PO-DRKI3102-G51VABSCHLVBNR       PIC S9(9) BINARY.
              10 PO-DRKI3102-G51VVRTRGBEGINN      PIC  X(008).
              10 PO-DRKI3102-G51VVRTRGENDE        PIC  X(008).
              10 PO-DRKI3102-G51VNACHNAME         PIC  X(030).
              10 PO-DRKI3102-G51VVORNAME          PIC  X(030).
              10 PO-DRKI3102-G51VGEBDAT           PIC  X(008).
              10 PO-DRKI3102-G51VANREDE           PIC S9(4) BINARY.
              10 PO-DRKI3102-G51VGESCHLECHT       PIC  X(001).
              10 PO-DRKI3102-G51VSTRASSE          PIC  X(040).
              10 PO-DRKI3102-G51VPLZ              PIC  X(009).
              10 PO-DRKI3102-G51VLANDID           PIC  X(003).
              10 PO-DRKI3102-G51VORT              PIC  X(030).
              10 PO-DRKI3102-G51VABRECHKZ         PIC  X(001).
              10 PO-DRKI3102-G51VZUSRAHMEN        PIC  X(001).
              10 PO-DRKI3102-G51VVORVENDDATUM     PIC  X(008).
              10 PO-DRKI3102-G51VVORVGES          PIC S9(4) BINARY.
              10 PO-DRKI3102-G51VVORVKUENDKZ      PIC  X(001).
      *
      ******* ENDE COPY-MEMBER ***************************************

