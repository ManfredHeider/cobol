      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2625.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2625.CPv  $
      *
      *   Rev 2.1   Aug 13 2012 08:53:30   t13ex15
      *AKI01141: DTA-Erweiterung Fondsdepot Bank "WpHG" Programme u
      *nd Copy
      *
      *   Rev 2.0   Jun 26 2012 07:52:10   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*#
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER zum Aufbereitungsprogramm für Aktualisierung    *
      *       WpHG(Bogen-)Daten FodB bzw. DeuBa                        *
      *       Es gibt keine Aenderungskennzeichen je Attribut, da      *
      *       immer alle Feldinhalte übernommen werden                 *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01141!21.06.12!1.0 ! Neu                         !SWZ        *
      *        !        !    !                             !           *
      *        !        !    !                             !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-SATZ.
      *
               10  'XX'-VERARB-KNZ               PIC X(001).
      *        Verarbeitungskennzeichen
      *        I = Insert U = Update D = Delete

               10  'XX'-GES                      PIC  X(002).
      *            GESELLSCHAFT
               10  'XX'-KEY.
                   15 'XX'-VNR                   PIC  9(013).
      *

               10 'XX'-WPHGBERUF-KNR             PIC S9(4) COMP.
               10 'XX'-WPHGANLZIEL-KNR           PIC S9(4) COMP.
               10 'XX'-WPHGANLDAUER-KNR          PIC S9(4) COMP.
               10 'XX'-WPHGANZKAUF-KNR           PIC S9(4) COMP.
               10 'XX'-WPHGORDERGR-KNR           PIC S9(4) COMP.
               10 'XX'-WPHGGESCHKRBASIS          PIC  X(0001).
               10 'XX'-WPHGHOEHEKRRAHM           PIC S9(6)V9(2) COMP-3.
               10 'XX'-WPHGWPDBERFREI            PIC  X(0001).
               10 'XX'-WPDIENSTANLBER            PIC  X(0001).
               10 'XX'-WPHGWPDVERMVERW           PIC  X(0001).
               10 'XX'-WPHGALSPRVERMAUF          PIC  X(0001).
               10 'XX'-WPHGALSRENTVORS           PIC  X(0001).
               10 'XX'-WPHGASGESRIVORS           PIC  X(0001).
               10 'XX'-WPHGALSWOHNEIGEN          PIC  X(0001).
               10 'XX'-WPHGALSKEINKZIEL          PIC  X(0001).
      *        ------
               10 'XX'-WPHGKTOGUTH-KNR           PIC S9(4) COMP.
               10 'XX'-WPHGWPVERM-KNR            PIC S9(4) COMP.
               10 'XX'-WPHGIMMOVERM-KNR          PIC S9(4) COMP.
               10 'XX'-WPHGSONSVERM-KNR          PIC S9(4) COMP.
               10 'XX'-WPHGVERBINDL-KNR          PIC S9(4) COMP.
               10 'XX'-WPHGNETTVERM-KNR          PIC S9(4) COMP.
               10 'XX'-WPHGNICHSELB-KNR          PIC S9(4) COMP.
               10 'XX'-WPHGSELBST-KNR            PIC S9(4) COMP.
               10 'XX'-WPHGKAPVERM-KNR   PIC S9(4) COMP.
               10 'XX'-WPHGWEITERE-KNR   PIC S9(4) COMP.
               10 'XX'-WPHGAUSGABEN-KNR   PIC S9(4) COMP.
               10 'XX'-WPHGFREIELIQ-KNR   PIC S9(4) COMP.
      *        ------
               10 'XX'-WPHGKER1RENTENF   PIC  X(0001).
               10 'XX'-WPHGKE1DAUER-KNR   PIC S9(4) COMP.
      *        ------
               10 'XX'-WPHGKER2RENTENF   PIC  X(0001).
               10 'XX'-WPHGKER2STRUKTWP   PIC  X(0001).
               10 'XX'-WPHGKE2DAUER-KNR   PIC S9(4) COMP.
      *        ------
               10 'XX'-WPHGKER3RENTENF   PIC  X(0001).
               10 'XX'-WPHGKER3AKTIENF   PIC  X(0001).
               10 'XX'-WPHGKER3STRUKTWP   PIC  X(0001).
               10 'XX'-WPHGKER3ALTINV   PIC  X(0001).
               10 'XX'-WPHGKE3DAUER-KNR   PIC S9(4) COMP.
      *        ------
               10 'XX'-WPHGKER4RENTENF   PIC  X(0001).
               10 'XX'-WPHGKER4AKTIENF   PIC  X(0001).
               10 'XX'-WPHGKER4STRUKTWP   PIC  X(0001).
               10 'XX'-WPHGKER4ALTINV   PIC  X(0001).
               10 'XX'-WPHGKE4DAUER-KNR   PIC S9(4) COMP.
      *        ------
               10 'XX'-WPHGKER5RENTENF   PIC  X(0001).
               10 'XX'-WPHGKER5AKTIENF   PIC  X(0001).
               10 'XX'-WPHGKER5STRUKTWP   PIC  X(0001).
               10 'XX'-WPHGKER5AITERMIN   PIC  X(0001).
               10 'XX'-WPHGKE5DAUER-KNR   PIC S9(4) COMP.
      *        ------
               10 'XX'-WPHGGPARTNER-KNR   PIC S9(4) COMP.
               10 'XX'-WPHGDATUNTERSCHR   PIC  X(010).
               10 'XX'-WPHGKEINEWPDNSTL   PIC  X(0001).
