      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0930.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0930.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:37:44   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0930 Mapsteuerung der Datenfelder        *
      *                          FüR DIALOG PO24                     *
      *                          'PROVISIONSDETAIL PROV-ERGEBNIS'    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    09.04.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!09.04.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE MAPSTEUERUNG DER DATENFELDER      *
      *    FÜR DIE TRANSAKTION PO24                                  *
      *                                                              *
      ****************************************************************

          30 MAP-STEUERUNG-PO24.
      ***    MAP-STEUERUNG MASKE 1
             35 MAP-ST-PRODART.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-DGV.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-PPFL-BEITRAG-PT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-PPFL-BEGINN.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-GSMT-PROV.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-GSMT-PROV-DAT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-PPFL-BEITRAG.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).


             35 MAP-ST-PPFL-SUMME.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).


             35 MAP-ST-GRUND-PROV.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).


             35 MAP-ST-EINH.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).


             35 MAP-ST-VSTAT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).


             35 MAP-ST-KD-BETR.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).


             35 MAP-ST-PG-PROV-STZ.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).


             35 MAP-ST-LAUFZEIT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-ZAHLWEISE.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-GRUND-PROV-STZ.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-MA-ABR-DAT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

      ***    MAP-STEUERUNG MASKE 2

             35 MAP-ST-HAFTUNG-DAUER.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-HAFTUNG-GSMT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-HAFTUNG-VON.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-HAFTUNG-BIS.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-KOND-GSMT-PROV-BL.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-PG-ABR-DAT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-PG-EINL-DAT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-UPD-DAT.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

             35 MAP-ST-UPD-KENNUNG.
                40 FELD-INTENSIV              PIC X(001).
                40 FELD-OFFEN                 PIC X(001).
                40 FELD-CURSOR-KZ             PIC X(001).
                40 FELD-AEND-KZ               PIC X(001).

      ******** Ende Copy-Member **************************************
