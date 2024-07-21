      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0123.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0123.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:46:18   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Feb 19 1997 12:19:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0123                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    FÜR DIE VSTATUS-SCHLÜSSELHILFE                             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHÄFER                                          *
      *    DATUM:    13.12.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0002!13.12.96! 01 ! NEU                       ! SCHÄFER  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DEN VARIABLEN KOMMUNIKATIONSBEREICH
      *    ZWISCHEN DIALOGEN
      *    - INPUT (VON AUFRUFENDEM DIALOG)
      *      ACHTUNG: AUF CADD-I-KOMM-VAR DARF NUR BEI
      *      !!!      DIALOG-INIT ZUGEGRIFFEN WERDEN; FALLS
      *      !!!      DIE INFOS SPAETER NOCH WICHTIG SIND,
      *      !!!      MUESSEN SIE IN CADI-SICHERUNG GESICHERT
      *      !!!      WERDEN.
      *      !!!      GRUND: DAS FELD WIRD EVTL. IN AUFGERUFENEN
      *      !!!      UNTERANWENDUNGEN UEBERSCHRIEBEN
      *
      ****************************************************************

       01  PI-DRAT0123.
           05  PI-DRAT0123-CADD-I-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0123-CADD-I-KOMM-VAR.

               10  PI-DRAT0123-FUNKTION               PIC  X(008).
                   88 PI-DRAT0123-SCHL-HILFE-VSTATUS  VALUE 'DRAT0123'.

       01  PO-DRAT0123.
           05  PO-DRAT0123-CADD-O-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0123-CADD-O-KOMM-VAR.

               10  PO-DRAT0123-CADD-VSTATUS           PIC  X(004).
               10  PO-DRAT0123-CADD-VSTATUS-TEXT      PIC  X(050).

      ******* ENDE COPY-MEMBER DRAT0123  ****************************
