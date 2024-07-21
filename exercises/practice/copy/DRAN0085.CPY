      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0085.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAN0085.CPv  $
      *
      *   Rev 2.5   Dec 18 2015 08:58:18   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.4   Oct 21 2015 14:40:18   t13ae23
      *AAN01223: Änderung Beginn/Ablauf AdvoCard
      *
      *   Rev 2.3   Oct 24 2013 11:01:30   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.2   Feb 01 2005 15:26:50   t13ae34
      *AAN00544: DB2+Batch: PoA Optimierung Advocard
      *
      *   Rev 2.1   Jul 25 2003 09:06:12   t13ae23
      *AAN00422 - Elektron. Antrag ADVOCARD -
      *
      *   Rev 2.0   Mar 18 2002 09:16:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0085  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00422!25.07.03!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00544!21.01.05!2.2 ! ADVOCARD OPTIMIERUNG QUOTE! HEIDER   *
      * AAN01073!24.10.13!2.3 ! SEPA                      ! RAHNER   *
      * AAN01223!08.12.15!2.5 ! BEGINN/ABLAUF ÄNDERBAR    ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0085                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE ADVC-ADVOCARD TBAN0005     *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0085.
           05 PI-DRAN0085-ADVC-KEY.
               10 PI-DRAN0085-PGEB-GES          PIC S9(4)  COMP.
               10 PI-DRAN0085-ADVC-HVANTNUM     PIC X(9).
               10 PI-DRAN0085-ADVC-VDABVERM     PIC S9(9)  COMP.
           05 PI-DRAN0085-ADVC-DATEN.
               10 PI-DRAN0085-ADVC-PIN          PIC S9(9) COMP.
               10 PI-DRAN0085-ADVC-HAND         PIC  X(1).
      *>>>>>
               10 PI-DRAN0085-ADVC-KONTONR-E   PIC S9(12)V COMP-3.
               10 PI-DRAN0085-ADVC-BLZ-E       PIC S9(9) COMP.
               10 PI-DRAN0085-ADVC-AMTLKZ-E    PIC X(12).
               10 PI-DRAN0085-ADVC-KNR-ZAHLW-E PIC S9(4) COMP.
               10 PI-DRAN0085-ADVC-KTY-ZAHLW-E PIC S9(4) COMP.
      *        10 PI-DRAN0085-ADVC-V-PLUS-E    PIC X.
      *<<<<<
               10 PI-DRAN0085-ADVC-IBAN-E      PIC X(34).
               10 PI-DRAN0085-ADVC-BIC-E       PIC X(11).
               10 PI-DRAN0085-ADVC-BEGINN-E    PIC X(10).
               10 PI-DRAN0085-ADVC-ABLAUF-E    PIC X(10).
      *
       01  PO-DRAN0085.
           05 PO-DRAN0085-RC                   PIC  X(003).
             88 PO-DRAN0085-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0085-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0085-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************



