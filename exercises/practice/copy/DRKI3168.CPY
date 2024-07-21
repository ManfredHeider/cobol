      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3168.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3168.CPv  $
      *
      *   Rev 2.2   Oct 11 2017 11:19:04   t13ex78
      *AKI01532: Programme & Copies und TWS 
      *
      *   Rev 2.0   Sep 18 2017 11:15:48   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER zum Aufbereitungsprogramm für Aktualisierung    *
      *       WpHG(Bogen-)Daten DeuBa inkl. Ergänzungen MIFID2         *
      *       Es gibt keine Aenderungskennzeichen je Attribut, da      *
      *       immer alle Feldinhalte übernommen werden                 *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01532!11.09.17!2.0 ! Neu                         ! tfl       *
      *        !        !    !                             !           *
      *        !        !    !                             !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-SATZ.
      *
               10  'XX'-VERARB-KNZ               PIC  X(001).
      *        Verarbeitungskennzeichen
      *        I = Insert U = Update D = Delete
 
               10  'XX'-GES                      PIC  X(002).
      *            GESELLSCHAFT
               10  'XX'-KEY.
                   15 'XX'-VNR                   PIC  9(013).
      *
 
               10 'XX'-WPHGBERUF-KNR             PIC S9(004) COMP.
               10 'XX'-WPHGANLZIEL-KNR           PIC S9(004) COMP.
               10 'XX'-WPHGANLDAUER-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGANZKAUF-KNR           PIC S9(004) COMP.
               10 'XX'-WPHGORDERGR-KNR           PIC S9(004) COMP.
               10 'XX'-WPHGGESCHKRBASIS          PIC  X(001).
               10 'XX'-WPHGHOEHEKRRAHM  PIC S9(006)V9(002) COMP-3.
               10 'XX'-WPHGWPDBERFREI            PIC  X(001).
               10 'XX'-WPDIENSTANLBER            PIC  X(001).
               10 'XX'-WPHGWPDVERMVERW           PIC  X(001).
               10 'XX'-WPHGALSPRVERMAUF          PIC  X(001).
               10 'XX'-WPHGALSRENTVORS           PIC  X(001).
               10 'XX'-WPHGASGESRIVORS           PIC  X(001).
               10 'XX'-WPHGALSWOHNEIGEN          PIC  X(001).
               10 'XX'-WPHGALSKEINKZIEL          PIC  X(001).
      *        ------
               10 'XX'-WPHGKTOGUTH-KNR           PIC S9(004) COMP.
               10 'XX'-WPHGWPVERM-KNR            PIC S9(004) COMP.
               10 'XX'-WPHGIMMOVERM-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGSONSVERM-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGVERBINDL-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGNETTVERM-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGNICHSELB-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGSELBST-KNR            PIC S9(004) COMP.
               10 'XX'-WPHGKAPVERM-KNR           PIC S9(004) COMP.
               10 'XX'-WPHGWEITERE-KNR           PIC S9(004) COMP.
               10 'XX'-WPHGAUSGABEN-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGFREIELIQ-KNR          PIC S9(004) COMP.
      *        ------
               10 'XX'-WPHGKER1RENTENF           PIC  X(001).
               10 'XX'-WPHGKE1DAUER-KNR          PIC S9(004) COMP.
      *        ------
               10 'XX'-WPHGKER2RENTENF           PIC  X(001).
               10 'XX'-WPHGKER2STRUKTWP          PIC  X(001).
               10 'XX'-WPHGKE2DAUER-KNR          PIC S9(004) COMP.
      *        ------
               10 'XX'-WPHGKER3RENTENF           PIC  X(001).
               10 'XX'-WPHGKER3AKTIENF           PIC  X(001).
               10 'XX'-WPHGKER3STRUKTWP          PIC  X(001).
               10 'XX'-WPHGKER3ALTINV            PIC  X(001).
               10 'XX'-WPHGKE3DAUER-KNR          PIC S9(004) COMP.
      *        ------
               10 'XX'-WPHGKER4RENTENF           PIC  X(001).
               10 'XX'-WPHGKER4AKTIENF           PIC  X(001).
               10 'XX'-WPHGKER4STRUKTWP          PIC  X(001).
               10 'XX'-WPHGKER4ALTINV            PIC  X(001).
               10 'XX'-WPHGKE4DAUER-KNR          PIC S9(004) COMP.
      *        ------
               10 'XX'-WPHGKER5RENTENF           PIC  X(001).
               10 'XX'-WPHGKER5AKTIENF           PIC  X(001).
               10 'XX'-WPHGKER5STRUKTWP          PIC  X(001).
               10 'XX'-WPHGKER5AITERMIN          PIC  X(001).
               10 'XX'-WPHGKE5DAUER-KNR          PIC S9(004) COMP.
      *        ------
               10 'XX'-WPHGGPARTNER-KNR          PIC S9(004) COMP.
               10 'XX'-WPHGDATUNTERSCHR          PIC  X(010).
               10 'XX'-WPHGKEINEWPDNSTL          PIC  X(001).
neu   *        ------
               10 'XX'-WPGHKER2MISCHFONDS        PIC  X(001).
               10 'XX'-WPGHKER2OIF               PIC  X(001).
               10 'XX'-WPGHKER2IMMOINVEST        PIC  X(001).
               10 'XX'-WPGHKER2KAPITALGESCH      PIC  X(001).
      *        ------
               10 'XX'-WPGHKER3KAPITALGESCH      PIC  X(001).
               10 'XX'-WPGHKER3KAPSCHUTZ90       PIC  X(001).
      *        ------
               10 'XX'-WPGHKER4RENTE-ETF         PIC  X(001).
               10 'XX'-WPGHKER4GELDMARKT         PIC  X(001).
               10 'XX'-WPGHKER4AKTIEN-ETF        PIC  X(001).
               10 'XX'-WPGHKER4MISCHFONDS        PIC  X(001).
               10 'XX'-WPGHKER4OIF               PIC  X(001).
               10 'XX'-WPGHKER4ROHSTOFF          PIC  X(001).
               10 'XX'-WPGHKER4SYNTH-ETF         PIC  X(001).
               10 'XX'-WPGHKER4-STR-AKT          PIC  X(001).
               10 'XX'-WPGHKER4-RK1-RK4          PIC  X(001).
               10 'XX'-WPGHKER4-RK5              PIC  X(001).
      *        ------
               10 'XX'-WPGHKER5ROHSTOFF          PIC  X(001).
               10 'XX'-WPGHKER5-AKTIEN           PIC  X(001).
               10 'XX'-WPGHKER5-ROHSTOFF         PIC  X(001).
               10 'XX'-WPGHKER5-STR-ETF          PIC  X(001).
      *        ------
               10 'XX'-WPGHKER6HEBEL             PIC  X(001).
               10 'XX'-WPHGKER6LONGS             PIC  X(001).
               10 'XX'-WPHGKE6DAUER-KNR          PIC S9(004) COMP.
      *        ------
               10 'XX'-WPHGKER7SHORTS            PIC  X(001).
               10 'XX'-WPHGKER7FUTURES           PIC  X(001).
               10 'XX'-WPHGKER7DERIVATE          PIC  X(001).
               10 'XX'-WPHGKER7DEVISEN           PIC  X(001).
               10 'XX'-WPHGKE7DAUER-KNR          PIC S9(004) COMP.
 

