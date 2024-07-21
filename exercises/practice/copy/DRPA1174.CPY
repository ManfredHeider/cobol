      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1174.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1174.CPv  $
      *
      *   Rev 2.1   May 25 1999 14:28:40   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 25 1999 14:10:30   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:03:22   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1174 FUNKTION GET ZUSTAND VERTRAGSDATEN- *
      *                          UND PROVISIONSDATENMODUL            *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    24.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!24.02.99! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1174                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1174.
             10 PI-DRPA1174-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.

       01 PO-DRPA1174.
          05 PO-DRPA1174-RC                    PIC  X(003).
             88 PO-DRPA1174-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1174-ZST.
            10 PO-DRPA1174-ZST-EVS             PIC  X(002).
             88 PO-DRPA1174-ZST-EVS-INIT                 VALUE '01'.
             88 PO-DRPA1174-ZST-EVS-GELESEN              VALUE '02'.
             88 PO-DRPA1174-ZST-EVS-A-AEND-OK            VALUE '03'.
             88 PO-DRPA1174-ZST-EVS-V-STR-AKT            VALUE '04'.
             88 PO-DRPA1174-ZST-EVS-VERTRAG-UB           VALUE '05'.

      *
      ******** Ende Copy-Member **************************************
