      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0407.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA0407.CPv  $
      *
      *   Rev 2.5   Jul 16 2015 15:07:22   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System:
      * Date
      *nmodellanpassungen - JWA
      *
      *   Rev 2.4   Jun 19 2012 09:23:34   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:48:28   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Dec 10 2007 09:20:50   t13ex21
      *APA00622: Programmänderungen Martina
      *
      *   Rev 2.1   Nov 02 2006 14:49:46   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 27 1999 13:27:46   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:32:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0407                                      *
      *    --------------------------------------------------------   *
      *    PMNA-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    DATUM:    09.12.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.12.97!01.01! NEU                      ! JÄGER    *
      * APA00012!28.05.98!01.02! NEUE ATTRIBUTE EINGEFÜGT ! JÄGER    *
      * APA00452!01.11.06! 2.1 ! DREIST. PRODUKTSCHLUESSEL! SCHEIDEL *
      * APA00622!26.07.07! 2.2 ! NEUES ATTRIBUT KNR-VGES  ! STORCH   *
      * APA01101!23.02.12! 2.3 ! VGES DURCH MANDANT ERSETZT! STORCH  *
      *         !        !     ! *-BEREICHE ENTFERNT      !          *
      * APA01932!05.05.15! 2.5 ! PG-UST-BETRAG,PG-USTSATZ ! WANG     *
      *         !        !     ! HINZU                    !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PNAP.                   *
      *                                                              *
      ****************************************************************
      *
             30 PNAP-DATEN.
                35 PNAP-ID               PIC S9(015)  PACKED-DECIMAL.
                35 PNAP-VERSION          PIC S9(004)  BINARY.
                35 KNR-OBJEKT-HKF-TAB    PIC S9(004)  BINARY.
                35 KNR-PNAP-OBJEKT       PIC S9(004)  BINARY.
                35 KTY-PNAP-ZUSTAND      PIC S9(004)  BINARY.
                35 KNR-PNAP-ZUSTAND      PIC S9(004)  BINARY.
                35 PNAP-FAELLIG-VON      PIC  X(010).
                35 PRINTE                PIC S9(015)  PACKED-DECIMAL.
                35 PGEB-GES              PIC S9(004)  BINARY.
                35 PNAP-VNR              PIC  X(015).
                35 PNAP-PIN              PIC S9(009)  BINARY.
                35 PVER-KUNDE-NAME       PIC  X(025).
                35 PNAP-GSMT-PROV        PIC S9(009)V99 PACKED-DECIMAL.
                35 PNAP-PG-ABR-DAT       PIC  X(010).
                35 PNAP-PROD-PROD-SCHL-1 PIC S9(004)  BINARY.
                35 PNAP-PROD-PROD-SCHL-2 PIC S9(004)  BINARY.
                35 PNAP-PROD-PROD-SCHL-3 PIC S9(004)  BINARY.
                35 PNAP-PROD-SCHL-KZ     PIC  X(001).
                35 PNAP-VSTAT            PIC  X(004).
                35 PNAP-VSTAT-UNR        PIC S9(004)  BINARY.
                35 KNR-MANDANT           PIC S9(004)  BINARY.
050515          35 PNAP-PG-UST-BETRAG    PIC S9(9)V9(2) USAGE COMP-3.
050515          35 PNAP-PG-UST-SATZ      PIC S9(003)V9(003).
      *
      ******* ENDE COPY-MEMBER **************************************

