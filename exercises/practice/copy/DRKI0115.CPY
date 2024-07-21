      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0115.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI0115.CPv  $
      *
      *   Rev 2.2   May 06 2003 13:53:42   p13sp16
      *Header-Korrektur
      *
      *   REV 2.1   OCT 21 1999 17:11:50   T13EX50
      *AKI0003 - Produktionsübergabe
      *
      *   REV 2.0   FEB 27 1999 12:52:58   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   12 AUG 1998 16:46:30   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   REV 1.0   29 SEP 1997 11:48:30   P13SP99
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0115  FUNKTION LESE-OZP                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: S.Jost                                          *
      *    DATUM:    12.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00003!12.02.98! 01 ! NEUERSTELLUNG             ! S.JOST   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI0115.
           05  PI-DRKI0115-MITARBPERSROLLE    PIC X(1).
           05  PI-DRKI0115-MITARBPERSONENID   PIC X(13).



      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI0115.
           05  PO-DRKI0115-RC                  PIC  X(03).
               88  PO-DRKI0115-RC-OK           VALUE 'OK '.
               88  PO-DRKI0115-RC-LEER         VALUE 'LER'.
               88  PO-DRKI0115-RC-EOF          VALUE 'EOF'.
               88  PO-DRKI0115-RC-NFD          VALUE 'NFD'.
           05  PO-DRKI0115-MITARBEITERNR  PIC S9(9) COMP.
      *
      ******** ENDE COPY-MEMBER (DRKI0115) *******************

