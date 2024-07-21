      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0107.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0107.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   26 Jun 1997 14:44:42   T13EX21
      *DAISY Release 1.0.3
      *
      *   Rev 1.1   Mar 06 1997 14:58:40   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0107  FUNKTIONEN "ERSTELLEN PEW      "   *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRÖMPELER                                       *
      *    DATUM:    26.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !26.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      * AFPA....!24.04.97!    ! NEU: Fkt. DRPA0262        ! Cecconi  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UPPA0010.
           05 PI-UPPA0010-FUNKTION            PIC X(008).
              88 PI-UPPA0010-DRPA0118             VALUE 'DRPA0118'.
              88 PI-UPPA0010-DRPA0262             VALUE 'DRPA0262'.
