      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0159.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0159.CPv  $
      *
      *   Rev 2.3   Jun 10 2008 10:29:34   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.2   Jul 06 2005 08:42:28   t13ae46
      *APA00363: Generali Bank Invest - Sparpläne
      *
      *   Rev 2.1   Jan 04 2000 13:06:20   T13AE18
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:28:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 10:21:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0159 DATENFELDER FÜR DIALOG SF43         *
      *                          'ZUORDNUNG PRODUKT/TARIF'           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    29.09.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA000  !29.09.99! 01 !NEU                        !BOHLINGER *
      * APA00363!27.06.05! 02 !PGTA-TARIF ERWEITERN       !WANG      *
      * APA00712!20.03.08!2.03!ERWEITERUNG UM MANDANT     ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIE LISTDATEN     *
      *    DES DIALOGMODULS FÜR DEN DIALOG SF43                      *
      *                                                              *
      ****************************************************************

      ***   DATENFELDER FÜR LISTDIALOG SF43
270605*     35 PGTA-TARIF                       PIC  X(012).
270605      35 PGTA-TARIF                       PIC  X(015).
            35 PGTA-GUELT-AB                    PIC  X(010).
            35 PGTA-GUELT-BIS                   PIC  X(010).
            35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
            35 PROD-BEZ                         PIC  X(040).
            35 PGTA-KENNUNG                     PIC  X(008).
            35 PGTA-LAST-UPDATE                 PIC  X(026).
200308      35 KNR-MANDANT                      PIC S9(004) BINARY.
      *
