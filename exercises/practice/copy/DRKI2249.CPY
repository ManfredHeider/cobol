      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2249.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2249.CPv  $
      *
      *   Rev 2.2   Apr 29 2009 14:58:22   t13ex14
      *AKI00818: Tabelle VERTRAGRABATT OZP-Objekte
      *
      *   Rev 2.1   06 May 2008 09:34:44   t13ex27
      *FKI00911: Verträge löschen- PMKI0413 (löschen DB2)
      *
      *   Rev 2.0   06 May 2008 08:17:28   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI2249  FUNKTIONEN LIST-OZP                *
      *                                                              *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00911!02.05.08!2.1 ! NEUERSTELLUNG             !I. Frenzel*
090409* AKI00817!09.04.09!    ! neue Felder f. Nachlässe  !W. Misgeld*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01 PI-DRKI2249.
          05 PI-DRKI2249-PROGRAMM               PIC X(008).
          05 PI-DRKI2249-UFKT                   PIC 9(002).
          05 PI-DRKI2249-DATEN.
            10 PI-DRKI2249-VRAVERTRAGSID        PIC X(13).

       01 PO-DRKI2249.
          05 PO-DRKI2249-DATEN.
            10 PO-DRKI2249-VRALFDNR               PIC S9(4) COMP.
            10 PO-DRKI2249-VRARABTEXT             PIC X(20).
            10 PO-DRKI2249-VRARABWERT             PIC S9(9)V9(2) COMP-3.
            10 PO-DRKI2249-VRARABWERWKZ           PIC X(01).
            10 PO-DRKI2249-VRARABEINHEIT          PIC S9(4) COMP.
            10 PO-DRKI2249-LETZTEAENDERUNG        PIC X(26).
090409      10 PO-DRKI1292-VRADAUERNACHLASS       PIC X(1).
>>          10 PO-DRKI1292-VRAKDBONUS-KNR         PIC S9(15)V COMP-3.
            10 PO-DRKI1292-VRAKDBONUS-KTY         PIC S9(4) COMP.
    <<      10 PO-DRKI1292-VRAOEFFDIENSTNL        PIC X(1).
090409      10 PO-DRKI1292-VRAVSPNACHLASS         PIC X(1).

          05 PO-DRKI2249-RC                       PIC  X(003).
             88 PO-DRKI2249-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2249-RC-EOF                       VALUE 'EOF'.
             88 PO-DRKI2249-RC-LEER                      VALUE 'LER'.
