      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0660.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0660.CPv  $
      *
      *   Rev 2.1   May 27 1999 16:11:40   T13EX26
      *MFE die 2.
      *
      *   Rev 2.0   May 26 1999 16:57:54   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:58:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0660  FUNKTION ALLE MELD ZU FÄLLIG       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    03.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *
       01 PI-DRPA0660.
          05 PI-DRPA0660-PROGRAMM            PIC  X(008).
          05 PI-DRPA0660-UFKT-CODE           PIC  9(002).
          05 PI-DRPA0660-DATEN.
             10 PI-DRPA0660-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0660-FAELLIG-VON             PIC  X(010).
             10 PI-DRPA0660-FAELLIG-BIS             PIC  X(010).
      *
       01 PO-DRPA0660.
          05 PO-DRPA0660-RC                         PIC  X(003).
             88 PO-DRPA0660-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0660-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0660-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA0660-DATEN.
             10 PO-DRPA0660-MELD.
                COPY DRPA0411.
      *
      ******* ENDE COPY-MEMBER **************************************
