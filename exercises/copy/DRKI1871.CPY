      ****************************************************************
      *    COPY-MEMBER: DRKI1871  LESEN VB-UE                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    14.03.2006                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00409!14.03.06! 2.0! NEUERSTELLUNG             ! SCHEIDEL *
      * APA02814!07.03.19! 2.2! KDU-ART ENTFERNT          ! SCHEIDEL *
      * DSY04009!06.09.21!    ! LFD-NR IN IN-/OUTPUT      ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1871                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN VBNR ZU GES, VNR, VBNR, KDU-ART  <<<           *
      *                                                              *
      ****************************************************************
        01 PI-DRKI1871.
           05 PI-DRKI1871-PGEB-GES     PIC S9(004)       BINARY.
           05 PI-DRKI1871-VNR          PIC  X(015).
           05 PI-DRKI1871-VBNR         PIC S9(009)       BINARY.
060921     05 PI-DRKI1871-LFDNR        PIC S9(004)       BINARY.
      *
        01 PO-DRKI1871.
           05 PO-DRKI1871-RC           PIC  X(003).
              88 PO-DRKI1871-RC-OK     VALUE 'OK '.
              88 PO-DRKI1871-RC-NOT-FOUND
                                       VALUE 'NFD'.
           05 PO-DRKI1871-VBNR         PIC S9(009)       BINARY.
           05 PO-DRKI1871-GUELTIG-AB   PIC  X(010).
060921     05 PO-DRKI1871-LFDNR        PIC S9(004)       BINARY.
      *


