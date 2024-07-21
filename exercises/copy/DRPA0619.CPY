      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0619.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0619.CPv  $
      *
      *   Rev 2.1   Mar 11 1999 10:34:16   T13EX22
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:55:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0619 ERMITTELN DER LETZTEN MELDUNG        *
      *                          ZUM UEBERGEBENEM FAELLIG-BIS         *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LETZTE MELD ZU FAELL>>                   *
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
      *    FÜR DIE FUNKTION DRPA0619                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0619.
          05 PI-DRPA0619-DATEN.
             10 PI-DRPA0619-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0619-FAELLIG-BIS             PIC  X(010).
      *
       01 PO-DRPA0619.
          05 PO-DRPA0619-RC                         PIC  X(003).
             88 PO-DRPA0619-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0619-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0619-DATEN.
             10 PO-DRPA0619-MELD.
                11 PO-DRPA0619-PMBE.
                   COPY DRPA0411.
                11 PO-DRPA0619-PMPR.
                   COPY DRPA0432.
                11 PO-DRPA0619-PMPT.
                   COPY DRPA0421.
      *
      ******* ENDE COPY-MEMBER **************************************

