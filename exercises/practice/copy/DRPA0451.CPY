      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0451.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0451.CPv  $
      *
      *   Rev 2.2   Feb 09 2016 15:48:58   t13ae37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.1   Feb 03 2016 14:57:30   T13AE37
      *APA02208: Anpssungen Korrekturprogramm USt
      *
      *   Rev 2.0   May 27 1999 13:54:32   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:38:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0451                                      *
      *    --------------------------------------------------------   *
      *    PDPT-PEPD-DATEN                                            *
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
      * AFPA0012!24.07.98! 01 ! PDPE-Daten explizit als   ! Foltz    *
      *         !        !    ! Tabelle                   !          *
      * APA02208!01.02.16!2.1 ! UMBAU WEGEN DATENMODELL-  ! BUCH     *
      *         !        !    ! ERWEITERUNG               !          *
      * APA02208!09.02.16!2.2 ! DIREKTE ZUGRIFFE ERLAUBEN ! BUCH     *
      *         !        !    ! NESTET COPYS              !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR PDPT MIT DEN            *
      *    ZUGEHÖRIGEN FÜR PDPE.                                     *
      *                                                              *
      ****************************************************************
      *
             25 PDPT-PDPE-DATEN.
                27 FILLER.
010216             COPY DRPA0571.
                27 MAX-USED-PDPE                    PIC S9(004) BINARY.
240798*         27 FILLER                           OCCURS 20
240798          27 PDPE-TAB.
240798            28 PDPE-TAB-DATEN                 OCCURS 20
                                                    INDEXED BY IX-PDPE.
                   COPY DRPA0415.
      *
      *
      ******* ENDE COPY-MEMBER **************************************
 

