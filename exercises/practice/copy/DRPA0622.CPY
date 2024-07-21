      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0622.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA0622.CPv  $
      *
      *   Rev 2.2   Apr 07 1999 09:31:08   T13EX44
      *Returncodes spezifischer definiert
      *
      *   Rev 2.1   Mar 12 1999 07:36:46   T13EX44
      * 
      *
      *   Rev 2.0   Feb 27 1999 14:29:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:55:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0622 ERMITTELN ALLE RUECKZUBELASTENDEN    *
      *                          MELDUNGEN AB UEBERGENEM FAELLIG-VON  *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<RUECKBEL. MELD FAELL>>                   *
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
      * AFPA0012!23.02.99! 01 ! RC erweitert              ! Scheidel *
      * AFPA0012!06.04.99! 01 ! RC SPEZIFIZIERT           ! SCHEIDEL *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0622                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0622.
          05 PI-DRPA0622-DATEN.
             10 PI-DRPA0622-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0622-FAELLIG-VON             PIC  X(010).
      *
       01 PO-DRPA0622.
          05 PO-DRPA0622-RC                         PIC  X(003).
             88 PO-DRPA0622-RC-OK                        VALUES 'OK '.
060499*                                                         'OKA',
060499*                                                         'OKB',
060499*                                                         'OKG',
060499*                                                         'OKN'.
230299       88 PO-DRPA0622-RC-OK-B-G                    VALUE  'OKA'.
230299       88 PO-DRPA0622-RC-OK-NUR-B                  VALUE  'OKB'.
230299       88 PO-DRPA0622-RC-OK-NUR-G                  VALUE  'OKG'.
230299       88 PO-DRPA0622-RC-OK-NUR-BNE                VALUE  'OKN'.
      *
             88 PO-DRPA0622-RC-NOT-FOUND                 VALUES 'NFD'.
060499*                                                         'NFE',
060499*                                                         'NFV'.
230299       88 PO-DRPA0622-RC-NOT-FOUND-NE              VALUE  'NFE'.
230299       88 PO-DRPA0622-RC-NOT-FOUND-VE              VALUE  'NFV'.
          05 PO-DRPA0622-DATEN.
             10 PO-DRPA0622-MELD.
                COPY DRPA0615.
      *
      ******* ENDE COPY-MEMBER **************************************
