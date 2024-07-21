      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0764.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0764.CPv  $
      *
      *   Rev 2.1   Jun 02 1999 13:51:54   T13EX44
      * 
      *
      *   Rev 2.0   May 26 1999 16:58:06   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0764  FUNKTION ERMITTELN ALLE PDPE'S     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PARENT                                          *
      *    DATUM:    23.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.12.97! 01 ! NEUERSTELLUNG             ! PARENT   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA0764.
          05 PI-DRPA0764-PROGRAMM                   PIC  X(008).
          05 PI-DRPA0764-UFKT-CODE                  PIC  9(002).
          05 PI-DRPA0764-DATEN.
             10 PI-DRPA0764-PDPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0764.
          05 PO-DRPA0764-RC                         PIC  X(003).
             88 PO-DRPA0764-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0764-RC-LEER                       VALUE 'LER'.
             88 PO-DRPA0764-RC-EOF                        VALUE 'EOF'.
          05 PO-DRPA0764-DATEN.
             10 PO-DRPA0764-PDPE.
                COPY DRPA0415.
      *
      ******* ENDE COPY-MEMBER **************************************
