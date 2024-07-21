      *---------------------------------------------------------------*
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0408.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0408.CPv  $
      *
      *   Rev 2.7   Mar 28 2017 08:13:52   t13ae37
      *APA02353: Neuer Produktpartner TeleSon - PVW
      *
      *   Rev 2.6   Jul 16 2015 15:07:20   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.5   Mar 10 2014 16:45:22   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.4   Jul 26 2013 11:40:44   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell
      *
      *   Rev 2.3   Jun 19 2012 09:23:42   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   May 21 2012 08:48:36   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.1   Dec 10 2007 09:26:22   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.0   Feb 27 1999 14:29:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 09:54:42   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:32:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0408                                      *
      *    --------------------------------------------------------   *
      *    PVER-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.10.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * AFPA0012!01.12.97!    ! PVER-AKT-FAELLIG-VON/-BIS ! WOLFF    *
      *         !        !    ! UMBENANNT                 !          *
      * AFPA0012!03.12.97!    ! NEU: KNR-PVER-AKZ         ! PRIGGEN  *
      *         !        !    ! UMBENANNT                 !          *
      * Apa00012!15.07.98!    ! NEU: LAST-UPDATE & KENNUNG! PARENT   *
      *         !        !    !                           !          *
      * APA00012!31.07.98!    ! NEU: AVERM & VM,          ! PARENT   *
      *         !        !    ! VORVERTRAG,VERSIONSKENNUNG!          *
      *         !        !    ! -GÜLTIG-AB UND -GUELTIGBIS!          *
      * APA00622!23.05.07!2.1 ! NEUES ATTRIBUT KNR-VGES   ! STORCH   *
      * APA01101!23.02.12!2.2 ! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    ! *-BEREICHE ENTFERNT       !          *
260713* APA01539!26.07.13!2.3 ! ERGÄNZUNGEN FÜR KALYPSO   ! RAHNER   *
      * APA01932!13.05.15!2.6 ! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02353!21.02.17!2.7 ! EINFÜHRUNG TeleSon        ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *                                                              *
      ****************************************************************
      *
             30 PVER-DATEN.
                35 PRINTE                PIC S9(015) PACKED-DECIMAL.
                35 PVER-VERSION          PIC S9(004) BINARY.
                35 PVER-F-GUELTIG-AB     PIC X(010).
                35 PVER-F-GUELTIG-BIS    PIC X(010).
                35 PVER-V-GUELTIG-AB     PIC X(026).
                35 PVER-V-GUELTIG-BIS    PIC X(010).
                35 PVER-V-KENNUNG        PIC X(008).
                35 KNR-PVER-ART          PIC S9(004) BINARY.
                35 PVER-VORVERTRAG       PIC X(001).
                35 PGEB-GES              PIC S9(004) BINARY.
                35 PVER-VNR              PIC X(015).
                35 PVER-KUNDE-NAME       PIC X(025).
                35 PVER-KUNDE-VNAME      PIC X(020).
                35 PVER-VERTR-BEGINN     PIC X(010).
                35 PVER-VERTR-ABLAUF     PIC X(010).
                35 PVER-PG-VBNR          PIC S9(009) BINARY.
                35 KNR-PVER-STAND        PIC S9(004) BINARY.
                35 PVER-VSTAT            PIC X(004).
                35 PVER-AKT-PIN          PIC S9(009) BINARY.
                35 PVER-AVERM            PIC S9(009) BINARY.
                35 PVER-VM               PIC S9(009) BINARY.
      *                                      PACKED-DECIMAL.
                35 PVER-PNAP-EINTRAG     PIC X(001).
                35 PVER-AKT-DGV          PIC S9(004) BINARY.
                35 PVER-AKT-PMBE-ID      PIC S9(015) PACKED-DECIMAL.
                35 PVER-AKT-FAELLIG-V    PIC X(010).
                35 PVER-AKT-FAELLIG-B    PIC X(010).
                35 PVER-LAST-UPDATE      PIC X(026).
                35 PVER-KENNUNG          PIC X(008).
                35 KNR-MANDANT           PIC S9(004) BINARY.
                35 KNR-AKZ-PVER          PIC S9(004) BINARY.
260713          35 PVER-BZ-NAME          PIC X(025).
260713          35 PVER-BZ-VNAME         PIC X(020).
260713          35 PVER-VP-NAME          PIC X(025).
260713          35 PVER-VP-VNAME         PIC X(020).
260713          35 KNR-FEIV-KZ           PIC S9(004) BINARY.
260713          35 KNR-VERM-GES          PIC S9(004) BINARY.
260713          35 PVER-UMW-NR           PIC S9(015) PACKED-DECIMAL.
260713          35 PVER-URSPR-POL-DAT    PIC X(010).
260713          35 PVER-VN-STRASSE       PIC X(035).
260713          35 PVER-VN-PLZ           PIC X(005).
260713          35 PVER-VN-ORT           PIC X(035).
130515          35 PVER-PG-UST-SATZ      PIC 9(003)V9(3) PACKED-DECIMAL.
210217          35 KNR-TLSN-PARTNER      PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
 

