      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1537.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1537.CPv  $
      *
      *   Rev 2.1   Sep 12 2002 14:54:08   t13ae46
      *APA00186: Central VNR-PIN-Zuordnungsdatensatz
      *
      *   Rev 2.0   04 Jul 2001 11:11:30   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1537  LIST-OZP                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    28.05.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00186!28.05.02! 01 ! NEUERSTELLUNG             ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1537                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
     *
             30 CENTRAL-VNR-PIN-ZUORDNUNGSDATENSATZ.
                35 SA                       PIC  X(001).
                35 SA-CKV                   PIC  X(003).
                35 DIREKTION                PIC  X(003).
                35 FILLER                   PIC  X(007).
                35 VNR                      PIC  X(015).
                35 PIN                      PIC  9(007).
                35 PRZ-PIN                  PIC  9(002).
                35 VBNR                     PIC  9(006).
                35 ERSTELLDAT               PIC  9(007) COMP-3.
                35 FILLER                   PIC  X(202).
      *
      ***** ENDE-COPY-MEMBER *****************************************

