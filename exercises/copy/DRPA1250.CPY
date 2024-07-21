      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1250.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1250.CPv  $
      *
      *   Rev 2.2   Jun 22 1999 23:13:00   T13AE18
      *apa00012
      *
      *   Rev 2.0   MAR 30 1999 08:44:14   P13SP99
      *NEUERSTELLUNG
      *
      *   Rev 2.0   MAR 05 1999 10:19:36   P13SP20
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1250 GET AVERM UND VM DES EVS-VERTRAGES  *
      *                          FÜR VERTRAGSDATENMODUL   UGPA0001   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    02.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.06.99!2.0 !NEU                        ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1250                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1250.
            10 PI-DRPA1250-PRINTE             PIC S9(015).
            10 PI-DRPA1250-PVER-VM            PIC S9(009) BINARY.
            10 PI-DRPA1250-PVER-AVERM         PIC S9(009) BINARY.

       01 PO-DRPA1250.
          05 PO-DRPA1250-RC                    PIC  X(003).
             88 PO-DRPA1250-RC-OK                       VALUE 'OK '.

      *---------------------------------------------------------------*
