      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0111.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0111.CPv  $
      *
      *   Rev 2.3   31 Oct 2007 17:01:10   t13ae30
      *AVB00905: PGM - Anpassung UWPA0012 TWI
      *
      *   Rev 2.2   Oct 31 2007 12:36:22   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jan 18 2000 10:02:56   T13AE18
      *APA00079
      *
      *
      *   Rev 2.0   Feb 27 1999 14:27:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:59:18   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0111  FUNKTIONEN FÜR UWPA0004            *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRÖMPELER                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !28.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      * APA00071!06.01.00!2.1 ! NEUE FUNKTION DRPA1327    ! BOHLINGER*
      * APA00622!11.06.07!2.2 ! NEUE FUNKTION DRPA1663    ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0004.
           05 PI-UWPA0004-FUNKTION            PIC X(008).
              88 PI-UWPA0004-DRPA0122             VALUE 'DRPA0122'.
060199        88 PI-UWPA0004-DRPA1327             VALUE 'DRPA1327'.
110607        88 PI-UWPA0004-DRPA1663             VALUE 'DRPA1663'.
