      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0059.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRAN0059.CPv  $
      *
      *   Rev 2.1   Jun 03 2002 18:13:46   t13ae34
      *AAN00357 - POA-AMV
      *
      *   Rev 2.0   Mar 18 2002 09:14:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0059                                     *
      *    FUNKTION   : POA -> VORVERTRAGSNUMMERN ZUORDNEN           *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00357!24.05.02!2.1  !NEU                        !HEIDER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0059                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0059.
           05 PI-DRAN0059-POAA-KEY.
               10 PI-DRAN0059-PGEB-GES        PIC  X(2).
               10 PI-DRAN0059-POA-PIN         PIC  S9(9) COMP.
               10 PI-DRAN0059-POA-STICHTAG    PIC   X(10).
               10 PI-DRAN0059-VVNR-TABELLE.
                  15 PI-DRAN0059-VVNR-TAB OCCURS 10
                     INDEXED BY PI-DRAN0059-I, PI-DRAN0059-I-2.
                     20 PI-DRAN0059-VVNR-T    PIC   X(14).
      *
       01 PO-DRAN0059.
          05 PO-DRAN0059-RC                      PIC  X(003).
             88 PO-DRAN0059-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0059-RC-NOK                    VALUE 'NOK'.

      ******** ENDE COPY-MEMBER **************************************
