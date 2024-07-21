      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0182.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0182.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:04:10   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0182  Verarbeitungsstatustabelle         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Wolff                                           *
      *    DATUM:    22.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !22.01.97! 01 ! Neuerstellung             ! Wolff    *
      * PEW-Rel.!        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  T-VSTAT-TABELLE.
           05  T-VSTAT-TAB            OCCURS 99
                                      INDEXED BY T-VTAB-IND.
               10  T-VTAB-VSTAT       PIC  X(004).
               10  T-VTAB-VSTAT-UNR   PIC S9(004) BINARY.
               10  T-VTAB-ORT         PIC  X(030).
               10  T-VTAB-BEREICH     PIC  X(010).
