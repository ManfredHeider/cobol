      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1432.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRPA1432.CPv  $
      *
      *   Rev 2.8   Nov 04 2019 14:17:06   t13ex26
      *APA02879: Baufinanzierung 2.0 - Anbindung einer Finanzierung
      *splattform (Provision)
      *
      *   Rev 2.7   Mar 16 2004 11:52:10   t13ex44
      *APA00270 - PEG für Ges. 39 und 69
      *
      *   Rev 2.6   Dec 22 2003 14:16:54   t13ae46
      *APA00262:Neue Funktion DRPA1566
      *
      *   Rev 2.5   Feb 20 2002 12:28:36   t13ex21
      *APA00172 - neue Funktion
      *
      *   Rev 2.4   Dec 05 2001 12:11:48   t13ex43
      *APA00167 : PEG Bank Austria (Sonstige Banken Österreich)
      *
      *   Rev 2.3   20 Jul 2001 09:40:26   t13ex21
      *APA00133 - Neue Funktion
      *
      *   Rev 2.2   20 Mar 2001 15:25:54   T13EX21
      *APA00131 - Neue Funktion
      *
      *   Rev 2.1   15 Nov 2000 16:09:20   T13EX43
      *APA00116 : Hypo-PEG umsetzen und einführen
      *
      *   Rev 2.0   12 Jul 2000 12:33:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1432  FUNKTIONEN UWPA0047
      *                 PM FORMAL PRÜFEN UND NORMIEREN
      *    -------------------------------------------------------
      *    ERSTELLT: KLEEMEIER
      *    DATUM:    08.11.00
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00116!08.11.00!2.1 ! NEU                       ! KLEEMEIER*
      * APA00131!21.02.01!2.2 ! NEUE FUNKTION DRPA1471    ! STORCH   *
      * APA00133!21.06.01!2.3 ! NEUE FUNKTION DRPA1494    ! STORCH   *
      * APA00167!22.11.01!2.4 ! NEUE FUNKTION DRPA1512    ! KLEEMEIER*
      * APA00172!15.01.02!2.5 ! NEUE FUNKTION DRPA1519    ! STORCH   *
      * APA00262!04.12.03!2.6 ! NEUE FUNKTION DRPA1566    ! WANG     *
      * APA00270!11.03.04!2.7 ! PEG GENERALI-BANK         ! SCHEIDEL *
      * APA02879!28.10.19!2.8 ! NEUE FUNKTION DRPA1937    ! KRANZ    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-UWPA0047.
           05 PI-UWPA0047-FUNKTION            PIC X(008).
              88 PI-UWPA0047-DRPA1431             VALUE 'DRPA1431'.
210201        88 PI-UWPA0047-DRPA1471             VALUE 'DRPA1471'.
210601        88 PI-UWPA0047-DRPA1494             VALUE 'DRPA1494'.
221101        88 PI-UWPA0047-DRPA1512             VALUE 'DRPA1512'.
150102        88 PI-UWPA0047-DRPA1519             VALUE 'DRPA1519'.
041203        88 PI-UWPA0047-DRPA1566             VALUE 'DRPA1566'.
110304        88 PI-UWPA0047-DRPA1571             VALUE 'DRPA1571'.
281019        88 PI-UWPA0047-DRPA1937             VALUE 'DRPA1937'.
      *
      ****** ENDE COPY-MEMBER ****************************************

