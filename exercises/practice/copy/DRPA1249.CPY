      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1249.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1249.CPv  $
      *
      *   Rev 2.1   Jun 05 1999 16:36:24   T13AE18
      *APA00012 Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:14   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:32   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1249 get averm und vm des evs-vertrages  *
      *                          FüR VERTRAGSDATENMODUL   UGPA0001   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    02.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * apa00012!02.06.99!2.0 !Neu                        ! Bohlinger*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1249                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1249.
             10 PI-DRPA1249-PRINTE             PIC S9(015)
                                                   PACKED-DECIMAL.

       01 PO-DRPA1249.
          05 PO-DRPA1249-RC                    PIC  X(003).
             88 PO-DRPA1249-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1249-DATEN.
            10 PO-DRPA1249-PVER-VM             PIC S9(009) BINARY.
            10 PO-DRPA1249-PVER-AVERM          PIC S9(009) BINARY.
            10 PO-DRPA1249-PVER-AVERM-PRZ      PIC  X(001).


