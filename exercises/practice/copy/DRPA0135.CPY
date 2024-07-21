      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0135.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0135.CPv  $
      *
      *   Rev 2.1   Nov 24 1999 09:45:12   T13AE34
      *AAN00234-Bündelung Sach DRPA0962
      *
      *   Rev 2.0   Feb 27 1999 14:27:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:02:32   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0135  FUNKTION "Produktgruppe ermitteln" *
      *                                                              *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGGEN                                        *
      *    DATUM:    09.09.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !09.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * AAN00234!23.11.99!2.1 ! DRPA0962                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZJPA0001.
           05 PI-ZJPA0001-FUNKTION            PIC X(008).
              88 PI-ZJPA0001-DRPA0136             VALUE 'DRPA0136'.
              88 PI-ZJPA0001-DRPA0139             VALUE 'DRPA0139'.
              88 PI-ZJPA0001-DRPA0962             VALUE 'DRPA0962'.
