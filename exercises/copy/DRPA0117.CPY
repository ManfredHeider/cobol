      *---------------------------------------------------------------*
      *    STANDARD F‹R ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0117.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0117.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 10:20:38   T13EX25
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 15:00:14   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0117  FUNKTION                           *
      *                           "Einheiten berechnen"              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGGEN                                        *
      *    DATUM:    11.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !11.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      *         !13.10.97! 01 ! NEUE FUNKTION DRPA0546    ! J‰GER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0015.
           05 PI-UPPA0015-FUNKTION            PIC X(008).
              88 PI-UPPA0015-DRPA0128             VALUE 'DRPA0128'.
              88 PI-UPPA0015-DRPA0546             VALUE 'DRPA0546'.
