      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1344.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1344.CPv  $
      *
      *   Rev 2.2   Mar 13 2000 14:48:54   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.1   Mar 09 2000 17:35:14   T13EX13
      *APA00087 Umstellung auf PPGZ 
      *
      *   Rev 2.0   Mar 01 2000 09:57:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1344 DATENFELDER FÜR                     *
      *                          'ZUORDNUNG PRODUKT-PGV'             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WILKENS                                         *
      *    DATUM:    01.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00087!03.01.00! 01 ! NEU                       ! WILKENS  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER DER ZUORDNUNG         *
      *    PRODUKT-PGV-ZUORDNUNG                                     *
      *                                                              *
      ****************************************************************
            35 KNR-PGV                  PIC S9(4) USAGE COMP.
            35 KNR-PVW-MODUS            PIC S9(4) USAGE COMP.
            35 PGEB-GES                 PIC S9(4) USAGE COMP.
            35 PROD-PROD-SCHL           PIC S9(4) USAGE COMP.
            35 PPGZ-GUELTIG-AB.
               40 PPGZ-GUELTIG-AB-JJJJ     PIC 9(4).
               40 PPGZ-GUELTIG-AB-MM       PIC 9(2).
               40 PPGZ-GUELTIG-AB-TT       PIC 9(2).
            35 PPGZ-GUELTIG-BIS.
               40 PPGZ-GUELTIG-BIS-JJJJ     PIC 9(4).
               40 PPGZ-GUELTIG-BIS-MM       PIC 9(2).
               40 PPGZ-GUELTIG-BIS-TT       PIC 9(2).
            35 KNR-PVW-GEVO          PIC S9(4) USAGE COMP.
            35 PPGZ-LAST-UPDATE      PIC X(26).
            35 PPGZ-KENNUNG          PIC X(8).
