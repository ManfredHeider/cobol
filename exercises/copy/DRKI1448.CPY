      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1448.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRKI1448.CPv  $
      *
      *   Rev 2.1   Jan 20 2003 09:45:14   t13ex27
      *AKI00303 - Kommissarische Kundenübertragung
      *
      *   Rev 2.0   Sep 12 2002 08:29:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1448  FUNKTION LESE-OZP                  *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00303!27.09.02!2.1 ! neu                       !I.Frenzel *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI1448.
           05  PI-DRKI1448-VERVERTRAGSID       PIC X(13).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI1448.
           05  PO-DRKI1448-RC                  PIC  X(03).
               88  PO-DRKI1448-RC-OK           VALUE 'OK '.
               88  PO-DRKI1448-RC-NFD          VALUE 'NFD'.

           05  PO-DRKI1448-VERVERTRAGSNUMMER   PIC  X(015).
           05  PO-DRKI1448-VERVERTRZUSTANDID   PIC S9(004) COMP.
      *
      ******** ENDE COPY-MEMBER (DRKI1448) *******************
