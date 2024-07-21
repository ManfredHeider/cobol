      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.05.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0712.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0712.CPv  $
      *
      *   Rev 2.3   Feb 22 2006 07:42:28   t13ex43
      *AVB00840: PGM - Jährliche §109a-Aktion 2005
      *
      *   Rev 2.2   Jun 03 2004 12:07:14   t13ex43
      *PGM 2 - Meldung §109A geänderter Satzaufbau
      *
      *   Rev 2.1   May 28 2004 17:22:56   t13ex43
      *PGM - Meldung §109A geänderter Satzaufbau
      *
      *   Rev 2.0   May 13 2004 09:42:46   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0712                                      *
      *    --------------------------------------------------------   *
      *    MELDUNGEN NACH § 109A (ELDA-SATZAUFBAU FÜR ÖSTERREICH)     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    13.05.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB00745!13.05.04!2.1 ! NEU                       ! KLEEMEIER *
030604* AVB00745!03.06.04!2.2 ! ELDA-INFOSATZ HINZU       ! KLEEMEIER *
080206* AVB00840!08.02.06!2.3 ! ÄNDERUNGEN FÜR 2005       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DER SATZ BESTEHT AUS EINEM FIXEN IDENT-TEIL (20 STELLEN)   *
      *    UND EINEM VARIABLEN TEIL (VORSATZ, MELDUNG, NACHSATZ).     *
      *****************************************************************
      *
       01 DRVB0712-SATZ.
      *
          05 DRVB0712-IDENTTEIL.
             10 DRVB0712-SART               PIC  X(002).
             10 DRVB0712-SANR               PIC  9(007).
             10 DRVB0712-UVST               PIC  X(002).
             10 DRVB0712-OBUS               PIC  9(007).
             10 DRVB0712-VSTR               PIC  X(002).
      *
          05 DRVB0712-DATENTEIL             PIC  X(980).
      *
          05 DRVB0712-VORSATZ     REDEFINES DRVB0712-DATENTEIL.
             10 DRVB0712-PROJ               PIC  X(002).
             10 DRVB0712-BEST               PIC  X(002).
             10 DRVB0712-DTNR               PIC  X(006).
             10 DRVB0712-EDAT               PIC  9(008).
             10 DRVB0712-EZEI               PIC  9(006).
             10 DRVB0712-HRST               PIC  X(045).
             10 DRVB0712-HKFZ               PIC  X(003).
             10 DRVB0712-HPLZ               PIC  X(007).
             10 DRVB0712-HORT               PIC  X(020).
             10 DRVB0712-HSTR               PIC  X(030).
             10 DRVB0712-VERS               PIC  9(002).
             10 DRVB0712-HTEL               PIC  X(020).
             10 DRVB0712-SOID               PIC  X(070).
      *
030604    05 DRVB0712-INFOSATZ    REDEFINES DRVB0712-DATENTEIL.
             10 DRVB0712-FSART-I            PIC  X(001).
             10 DRVB0712-CLEAR-I            PIC  X(006).
             10 DRVB0712-CLDVR-I            PIC  X(007).
080206***    10 DRVB0712-FANRL-I            PIC  X(002).
080206***    10 DRVB0712-STNRL-I            PIC  X(007).
080206***    10 DRVB0712-DVRNL-I            PIC  X(007).
080206***    10 DRVB0712-FANRA-I            PIC  X(002).
080206***    10 DRVB0712-STNRA-I            PIC  X(007).
080206***    10 DRVB0712-DVRNA-I            PIC  X(007).
080206       10 DRVB0712-FANRL-I            PIC  9(002).
080206       10 DRVB0712-STNRL-I            PIC  9(007).
080206       10 DRVB0712-DVRNL-I            PIC  9(007).
080206       10 DRVB0712-FANRA-I            PIC  9(002).
080206       10 DRVB0712-STNRA-I            PIC  9(007).
080206       10 DRVB0712-DVRNA-I            PIC  9(007).
             10 DRVB0712-ARTD-I             PIC  X(002).
             10 DRVB0712-DTUE-I             PIC  9(008).
             10 DRVB0712-ZTUE-I             PIC  9(006).
             10 DRVB0712-STVE-I             PIC  9(002).
             10 DRVB0712-ANAM-I             PIC  X(037).
             10 DRVB0712-ATIT-I             PIC  X(020).
             10 DRVB0712-AADR-I             PIC  X(037).
             10 DRVB0712-ALKZ-I             PIC  X(003).
             10 DRVB0712-APLZ-I             PIC  X(010).
             10 DRVB0712-AORT-I             PIC  X(030).
             10 DRVB0712-INTE-I             PIC  X(011).
             10 DRVB0712-JAHR-I             PIC  9(004).
             10 DRVB0712-GESA-I             PIC  9(007).
             10 DRVB0712-ANZA-I             PIC  9(007).
      *
          05 DRVB0712-MELDUNG     REDEFINES DRVB0712-DATENTEIL.
             10 DRVB0712-FSART              PIC  X(001).
             10 DRVB0712-CLADR              PIC  X(006).
             10 DRVB0712-CLDVR              PIC  X(007).
             10 DRVB0712-FANRL              PIC  9(002).
             10 DRVB0712-STNRL              PIC  9(007).
             10 DRVB0712-DVRNL              PIC  9(007).
             10 DRVB0712-ARTU               PIC  X(001).
             10 DRVB0712-DTUE               PIC  9(008).
             10 DRVB0712-ZTUE               PIC  9(006).
             10 DRVB0712-REFN               PIC  X(030).
             10 DRVB0712-INTE               PIC  X(002).
             10 DRVB0712-FEHL               PIC  X(020).
             10 DRVB0712-STAT               PIC  X(001).
             10 DRVB0712-FIND               PIC  X(001).
             10 DRVB0712-JAHR               PIC  9(004).
             10 DRVB0712-AVSN               PIC  9(010).
             10 DRVB0712-AFAN               PIC  X(002).
             10 DRVB0712-ASTN               PIC  X(007).
             10 DRVB0712-ANAM               PIC  X(037).
             10 DRVB0712-ATIT               PIC  X(020).
             10 DRVB0712-AADR               PIC  X(037).
             10 DRVB0712-ALKZ               PIC  X(003).
             10 DRVB0712-APLZ               PIC  X(010).
             10 DRVB0712-AORT               PIC  X(030).
             10 DRVB0712-ARTE               PIC  X(002).
             10 DRVB0712-V341               PIC  X(001).
             10 DRVB0712-B341               PIC  9(008)V99.
             10 DRVB0712-V344               PIC  X(001).
             10 DRVB0712-B344               PIC  9(008)V99.
             10 DRVB0712-FANRA              PIC  9(002).
             10 DRVB0712-STNRA              PIC  9(007).
             10 DRVB0712-DVRNA              PIC  X(007).
             10 DRVB0712-TELN               PIC  X(020).
             10 DRVB0712-FAXN               PIC  X(020).
             10 DRVB0712-KORR               PIC  X(001).
             10 DRVB0712-WOBD               PIC  X(012).
             10 DRVB0712-ZOBD               PIC  X(015).
      *
          05 DRVB0712-NACHSATZ    REDEFINES DRVB0712-DATENTEIL.
             10 DRVB0712-SANZ               PIC  9(006).
      *
      ***** ENDE COPY-MEMBER DRVB0712 *****************************
      *
