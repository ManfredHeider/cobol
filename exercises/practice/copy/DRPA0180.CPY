      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0180.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0180.CPv  $
      *
      *   Rev 2.1   Mar 20 2002 10:22:10   t13ex21
      *FPA00368 - Neue Funktion DRPA1529
      *
      *   Rev 2.0   Feb 27 1999 14:28:08   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 09:23:06   T13EX16
      *APA00032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 15:04:00   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0180  Funktionen für UPPA0025            *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Wolff                                           *
      *    DATUM:    23.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !23.01.97! 01 ! Neuerstellung             ! Wolff    *
      * PEW-Rel.!        !    !                           !          *
      * AFPA0012!16.05.97!    ! Zusätzliche Funktionen    ! Mund     *
      *         !        !    ! DRPA0291-DRPA0294         !          *
      *         !        !    !                           !          *
      * AFPA0012!24.11.97!    ! NEU: DRPA0641             ! WOLFF    *
      * FPA00368!19.03.02!2.01! NEUE FUNKTION DRPA1529    ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0025.
           05 PI-UPPA0025-FUNKTION            PIC X(008).
              88 PI-UPPA0025-DRPA0181             VALUE 'DRPA0181'.
160597        88 PI-UPPA0025-DRPA0291             VALUE 'DRPA0291'.
160597        88 PI-UPPA0025-DRPA0292             VALUE 'DRPA0292'.
160597        88 PI-UPPA0025-DRPA0293             VALUE 'DRPA0293'.
160597        88 PI-UPPA0025-DRPA0294             VALUE 'DRPA0294'.
210597        88 PI-UPPA0025-DRPA0296             VALUE 'DRPA0296'.
241197        88 PI-UPPA0025-DRPA0641             VALUE 'DRPA0641'.
190302        88 PI-UPPA0025-DRPA1529             VALUE 'DRPA1529'.
