      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0620.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0620.CPv  $
      *
      *   Rev 2.1   Mar 15 1999 16:15:20   T13EX22
      *MFE-UMSTELLUNG
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:55:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0620 ERMITTELN DER EINEN MELDUNG ZU       *
      *                          VORGEGEBENEM FAELLIG-VON -BIS        *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<DIE MELD ZU FAELLIG >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.11.97! 01 ! NEU                       ! PRIGGEN  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0620                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0620.
          05 PI-DRPA0620-DATEN.
             10 PI-DRPA0620-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0620-FAELLIG-VON             PIC  X(010).
             10 PI-DRPA0620-FAELLIG-BIS             PIC  X(010).
      *
       01 PO-DRPA0620.
          05 PO-DRPA0620-RC                         PIC  X(003).
             88 PO-DRPA0620-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0620-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0620-DATEN.
             10 PO-DRPA0620-MELD.
                11 PO-DRPA0620-PMBE.
                   COPY DRPA0411.
                11 PO-DRPA0620-PMPR.
                   COPY DRPA0432.
                11 PO-DRPA0620-PMPT.
                   COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************
