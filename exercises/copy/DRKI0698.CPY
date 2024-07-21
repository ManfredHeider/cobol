      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0698.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0698.CPv  $
      *
      *   Rev 2.5   Oct 26 2009 07:36:54   t13ex14
      *AKI00846: Erw. der Bestands-DTAs Badenia: Programme u. Auftr
      *äge
      *
      *   Rev 2.4   Aug 29 2008 11:11:34   t13ex14
      *AKI00737: Badenia-Zwischen-und Vorfinanzierungen; Vertragsda
      *ten
      *
      *   Rev 2.3   11 Aug 2007 12:41:46   t13ae31
      *AKI00623: Erweiterung des Bestands-DTA von der Badenia
      *
      *   Rev 2.2   Jun 30 1999 09:50:30   T13EX51
      *AKI00030 Einfügen WKZ
      *
      *   Rev 2.1   Apr 08 1999 15:48:26   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:36   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   25 Aug 1998 09:31:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 6 9 8    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER für BADENIA-DAten die in die Tabelle              *
      *              GUTHABENPRODUKT                                   *
      *      eingestellt werden.                                       *
      *                                                                *
      *    SATZLÄNGE    60                                             *
200207*    SATZLÄNGE    70 - neue Produkte Finanzierungen
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *30.09.98!NEUERSTELLUNG                          !Dobslaff ! 01  *
      *KI-NEU  !                                       !         !     *
      *        !                                       !         !     *
      *AKI00030!Einfügen Währungskennzeichen           !Dobslaff ! 02  *
      *        !                                       !         !     *
120808*AKI00737! neues Feld GHP-LFDNR für ZWK/BVD      !W.Misgeld!
      *        !                                       !         !
140909*AKI00846! DTA-Erweiterung Badenie; Finaz.-KZ    !WMI      !
      *        !                                       !         !
      *        !                                       !         !     *
      ******************************************************************
      *
       01  DRKI0698.
      *
           05  GHP-GES                     PIC  9(002).
120808*    05  GHP-VNR                     PIC  9(015).
120808     05  GHP-VNR-LFDNR.
120808          10 GHP-VNR                 PIC  9(015).
120808          10 GHP-LFDNR               PIC  9(001).
120808*         1 = Bausparvertrag
120808*         2 = ZWK (Zwischenkredit)/BVD (Bauvorausdarlehen)
      *
      ***    05  GHP-PRODUKT-ID              PIC  9(002).
200207     05  GHP-PRODUKT-ID              PIC  9(003).
           05  GHP-PRODUKT-ID-AEKZ         PIC  X(001).
      *
           05  GHP-ANLAGEN-NAME            PIC  X(015).
           05  GHP-ANLAGEN-NAME-AEKZ       PIC  X(001).
      *
           05  GHP-ZINSSATZ-ALLG           PIC  9(003)V99.
           05  GHP-ZINSSATZ-ALLG-AEKZ      PIC  X(001).
      *        J WENN ZINSSATZ-ALLGEMEIN GEAENDERT
      *
      *
           05  GHP-INFOSPERRE              PIC  X(001).
      *
           05  GHP-VERFUEGUNG              PIC  X(002).
      *
           05  GHP-GUTHABEN                PIC  S9(009)V99.
           05  GHP-GUTHABEN-AEKZ           PIC  X(001).
      *
           05  GHP-WKZ                     PIC  X(001).
           05  GHP-WKZ-AEKZ                PIC  X(001).
      *
140909     05  GHP-FINANZIERNGKZ           PIC  X(001).
140909     05  GHP-FINANZIERNGKZ-AEKZ      PIC  X(001).
140909*        J WENN GEAENDERT
130808*    05  FILLER                      PIC  X(009).
130808*    05  FILLER                      PIC  X(008).
140909     05  FILLER                      PIC  X(006).
           05  GHP-AKTUAL-KZ               PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

      ******************************************************************
